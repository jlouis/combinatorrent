module Process.PieceMgr
    ( PieceMgrMsg(..)
    , PieceMgrChannel
    , Blocks(..)
    , start
    , createPieceDb
    )
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (assert)

import Control.Monad.Reader
import Control.Monad.State
import Data.List
import qualified Data.PendingSet as PendS
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.PieceSet as PS

import Prelude hiding (log)

import System.Random
import System.Random.Shuffle

import Process.FS hiding (start)
import Process.Status as STP hiding (start) 
import Process.ChokeMgr (ChokeMgrMsg(..), ChokeMgrChannel)
import Supervisor
import Torrent
import Tracer
import Process

----------------------------------------------------------------------

-- | The Piece Database tracks the current state of the Torrent with respect to pieces.
--   In the database, we book-keep what pieces are missing, what are done and what are
--   currently in the process of being downloaded. The crucial moment is when we think
--   we have a full piece: we check it against its SHA1 and if it is good, we can mark
--   that piece as done.
--
--   Better implementations for selecting among the pending Pieces is probably crucial
--   to an effective client, but we keep it simple for now.
data ST = ST
    { pendingPieces :: !PS.PieceSet -- ^ Pieces currently pending download
    , donePiece     :: !PS.PieceSet -- ^ Pieces that are done
    , donePush      :: ![ChokeMgrMsg] -- ^ Pieces that should be pushed to the Choke Mgr.
    , inProgress    :: !(M.Map PieceNum InProgressPiece) -- ^ Pieces in progress
    , downloading   :: ![(PieceNum, Block)]    -- ^ Blocks we are currently downloading
    , infoMap       :: !PieceMap   -- ^ Information about pieces
    , endGaming     :: !Bool       -- ^ If we have done any endgame work this is true
    , histogram     :: !PendS.PendingSet -- ^ Track the rarity of pieces
    , assertCount   :: !Int        -- ^ When to next check the database for consistency
    , traceBuffer   :: !(Tracer String)
    }

sizeReport :: Process CF ST String
sizeReport = do
    (ST pend done dpush progress down _ _ histo _ _) <- get
    p1sz <- PS.size pend
    p2sz <- PS.size done
    return $ show
         [ ("Pending", p1sz)
         , ("Done"   , p2sz)
         , ("DonePush", length dpush)
         , ("InProgress", M.size progress)
         , ("Downloading", length down)
         , ("Histo", PendS.size histo) ]

-- | The InProgressPiece data type describes pieces in progress of being downloaded.
--   we keep track of blocks which are pending as well as blocks which are done. We
--   also keep track of a count of the blocks. When a block is done, we cons it unto
--   @ipHaveBlocks@. When @ipHave == ipDone@, we check the piece for correctness. The
--   field @ipHaveBlocks@ could in principle be omitted, but for now it is kept since
--   we can use it for asserting implementation correctness. We note that both the
--   check operations are then O(1) and probably fairly fast.
data InProgressPiece = InProgressPiece
    { ipDone  :: Int -- ^ Number of blocks when piece is done
    , ipHaveBlocks :: S.Set Block -- ^ The blocks we have
    , ipPendingBlocks :: [Block] -- ^ Blocks still pending
    } deriving Show

-- INTERFACE
----------------------------------------------------------------------

-- | When the PieceMgrP returns blocks to a peer, it will return them in either
--   "Leech Mode" or in "Endgame mode". The "Leech mode" is when the client is
--   leeching like normal. The "Endgame mode" is when the client is entering the
--   endgame. This means that the Peer should act differently to the blocks.
data Blocks = Leech [(PieceNum, Block)]
            | Endgame [(PieceNum, Block)]

-- | Messages for RPC towards the PieceMgr.
data PieceMgrMsg = GrabBlocks Int PS.PieceSet (TMVar Blocks)
                   -- ^ Ask for grabbing some blocks
                 | StoreBlock PieceNum Block B.ByteString
                   -- ^ Ask for storing a block on the file system
                 | PutbackBlocks [(PieceNum, Block)]
                   -- ^ Put these blocks back for retrieval
                 | AskInterested PS.PieceSet (TMVar Bool)
                   -- ^ Ask if any of these pieces are interesting
                 | GetDone (TMVar [PieceNum])
                   -- ^ Get the pieces which are already done
                 | PeerHave [PieceNum]
                   -- ^ A peer has the given piece(s)
                 | PeerUnhave [PieceNum]
                   -- ^ A peer relinquished the given piece Indexes

instance Show PieceMgrMsg where
    show (GrabBlocks x _ _) = "GrabBlocks " ++ show x
    show (StoreBlock pn blk _) = "StoreBlock " ++ show pn ++ " " ++ show blk
    show (PutbackBlocks x)     = "PutbackBlocks " ++ show x
    show (AskInterested _ _)   = "AskInterested"
    show (GetDone _)           = "GetDone"
    show (PeerHave xs)         = "PeerHave " ++ show xs
    show (PeerUnhave xs)       = "PeerUnhave " ++ show xs

type PieceMgrChannel = TChan PieceMgrMsg

data CF = CF
    { pieceMgrCh :: PieceMgrChannel
    , fspCh :: FSPChannel
    , chokeCh :: ChokeMgrChannel
    , statusCh :: StatusChannel
    , pMgrInfoHash :: InfoHash
    }

instance Logging CF where
  logName _ = "Process.PieceMgr"

type PieceMgrProcess v = Process CF ST v

start :: PieceMgrChannel -> FSPChannel -> ChokeMgrChannel -> StatusChannel -> ST -> InfoHash
      -> SupervisorChan -> IO ThreadId
start mgrC fspC chokeC statC db ih supC =
    {-# SCC "PieceMgr" #-}
    spawnP (CF mgrC fspC chokeC statC ih) db
                    (catchP (forever pgm)
                        (defaultStopHandler supC))
  where pgm = do
          assertST
          rpcMessage
          drainSend

drainSend :: Process CF ST ()
drainSend = do
    dl <- gets donePush
    if (null dl)
        then return ()
        else sendChokeMgr (head dl) >> drainSend

sendChokeMgr :: ChokeMgrMsg -> Process CF ST ()
sendChokeMgr e = do
    c <- asks chokeCh
    liftIO . atomically $ writeTChan c e
    modify (\db -> db { donePush = tail (donePush db) })

traceMsg :: PieceMgrMsg -> Process CF ST ()
traceMsg m = modify (\db -> db { traceBuffer = trace (show m) (traceBuffer db) })

rpcMessage :: Process CF ST ()
rpcMessage = do
    ch <- asks pieceMgrCh
    m <- liftIO . atomically $ readTChan ch
    traceMsg m
    case m of
      GrabBlocks n eligible c ->
          do blocks <- grabBlocks n eligible
             liftIO . atomically $ do putTMVar c blocks -- Is never supposed to block
      StoreBlock pn blk d -> storeBlock pn blk d
      PutbackBlocks blks -> mapM_ putbackBlock blks
      GetDone c -> do
         done <- PS.toList =<< gets donePiece
         liftIO . atomically $ do putTMVar c done -- Is never supposed to block either
      PeerHave idxs -> peerHave idxs
      PeerUnhave idxs -> peerUnhave idxs
      AskInterested pieces retC -> do
         intr <- askInterested pieces
         liftIO . atomically $ do putTMVar retC intr -- And this neither too!


storeBlock :: PieceNum -> Block -> B.ByteString -> Process CF ST ()
storeBlock pn blk d = do
   debugP $ "Storing block: " ++ show (pn, blk)
   fch <- asks fspCh
   liftIO . atomically $ writeTChan fch $ WriteBlock pn blk d
   modify (\s -> s { downloading = downloading s \\ [(pn, blk)] })
   endgameBroadcast pn blk
   done <- updateProgress pn blk
   when done
       (do assertPieceComplete pn
           pend <- gets pendingPieces
           iprog <- gets inProgress
           pendSz <- PS.size pend
           infoP $ "Piece #" ++ show pn
                     ++ " completed, there are "
                     ++ (show pendSz) ++ " pending "
                     ++ (show $ M.size iprog) ++ " in progress"
           l <- gets infoMap >>=
                (\pm -> case M.lookup pn pm of
                                Nothing -> fail "Storeblock: M.lookup"
                                Just x -> return $ len x)
           ih <- asks pMgrInfoHash
           c <- asks statusCh
           liftIO . atomically $ writeTChan c (CompletedPiece ih l)
           pieceOk <- checkPiece pn
           case pieceOk of
             Nothing ->
                    do fail "PieceMgrP: Piece Nonexisting!"
             Just True -> do completePiece pn
                             markDone pn
                             checkFullCompletion
             Just False -> putbackPiece pn)

askInterested :: PS.PieceSet -> Process CF ST Bool
askInterested pieces = do
    inProg <- M.keys <$> gets inProgress
    amongProg <- filterM (flip PS.member pieces) inProg
    if (not $ null amongProg)
        then return True
        else do
            pend   <- gets pendingPieces
            intsct <- PS.intersection pieces pend
            return (not $ null intsct)

peerHave :: [PieceNum] -> Process CF ST ()
peerHave idxs = modify (\db -> db { histogram = PendS.haves idxs (histogram db)})

peerUnhave :: [PieceNum] -> Process CF ST ()
peerUnhave idxs = modify (\db -> db { histogram = PendS.unhaves idxs (histogram db)})

endgameBroadcast :: PieceNum -> Block -> Process CF ST ()
endgameBroadcast pn blk = do
    ih <- asks pMgrInfoHash
    gets endGaming >>=
      flip when (modify (\db -> db { donePush = (BlockComplete ih pn blk) : donePush db }))

markDone :: PieceNum -> Process CF ST ()
markDone pn = do
    ih <- asks pMgrInfoHash
    modify (\db -> db { donePush = (PieceDone ih pn) : donePush db })

checkPiece :: PieceNum -> Process CF ST (Maybe Bool)
checkPiece n = do
    v <- liftIO newEmptyTMVarIO
    fch <- asks fspCh
    liftIO $ do
        atomically $ writeTChan fch $ CheckPiece n v
        atomically $ takeTMVar v

-- HELPERS
----------------------------------------------------------------------

createPieceDb :: MonadIO m => PiecesDoneMap -> PieceMap -> m ST
createPieceDb mmap pmap = do
    pending <- filt (==False)
    done    <- filt (==True)
    return $ ST pending done [] M.empty [] pmap False PendS.empty 0 (Tracer.new 20)
  where
    filt f  = PS.fromList (M.size pmap) . M.keys $ M.filter f mmap

----------------------------------------------------------------------

-- | The call @completePiece db pn@ will mark that the piece @pn@ is completed
completePiece :: PieceNum -> PieceMgrProcess ()
completePiece pn = do
    PS.insert pn =<< gets donePiece
    modify (\db -> db { inProgress = M.delete pn (inProgress db) })

-- | Handle torrent completion
checkFullCompletion :: PieceMgrProcess ()
checkFullCompletion = do
    doneP <- gets donePiece
    im    <- gets infoMap
    donePSz <- PS.size doneP
    when (M.size im == donePSz)
        (do liftIO $ putStrLn "Torrent Completed"
            ih <- asks pMgrInfoHash
            asks statusCh >>= (\ch -> liftIO . atomically $ writeTChan ch (STP.TorrentCompleted ih))
            c <- asks chokeCh
            liftIO . atomically $ writeTChan c (TorrentComplete ih))

-- | The call @putBackPiece db pn@ will mark the piece @pn@ as not being complete
--   and put it back into the download queue again.
putbackPiece :: PieceNum -> PieceMgrProcess ()
putbackPiece pn = do
    PS.insert pn =<< gets pendingPieces
    modify (\db -> db { inProgress = M.delete pn (inProgress db) })

-- | Put back a block for downloading.
--   TODO: This is rather slow, due to the (\\) call, but hopefully happens rarely.
putbackBlock :: (PieceNum, Block) -> PieceMgrProcess ()
putbackBlock (pn, blk) = do
    done <- gets donePiece
    doneMember <- PS.member pn done
    unless (doneMember) -- Happens at endgame, stray block
      $ modify (\db -> db { inProgress = ndb (inProgress db)
                          , downloading = downloading db \\ [(pn, blk)]})
  where ndb db = M.alter f pn db
        -- In endgame, the first will never happen. If it is done, the doneMember
        -- check above should take care of the problem. If the block has been downloaded
        -- by another peer in endgame, there is nothing to do.
        --
        -- Otherwise, we put the block back as pending. If in endgame, the next request
        -- will pull it into downloading again for endgaming.
        f Nothing     = fail "The 'impossible' happened"
        f (Just ipp)
            | S.member blk (ipHaveBlocks ipp) = Just ipp
            | otherwise = Just ipp {ipPendingBlocks = blk : ipPendingBlocks ipp}

-- | Assert that a Piece is Complete. Can be omitted when we know it works
--   and we want a faster client.
assertPieceComplete :: PieceNum -> PieceMgrProcess ()
assertPieceComplete pn = do
    inprog <- gets inProgress
    ipp <- case M.lookup pn inprog of
                Nothing -> fail "assertPieceComplete: Could not lookup piece number"
                Just x -> return x
    dl <- gets downloading
    pm <- gets infoMap
    sz <- case M.lookup pn pm of
            Nothing -> fail "assertPieceComplete: Could not lookup piece in piecemap"
            Just x -> return $ len x
    unless (assertAllDownloaded dl pn)
      (fail "Could not assert that all pieces were downloaded when completing a piece")
    unless (assertComplete ipp sz)
      (fail $ "Could not assert completion of the piece #" ++ show pn
                ++ " with block state " ++ show ipp)
  where assertComplete ip sz = checkContents 0 (fromIntegral sz) (S.toAscList (ipHaveBlocks ip))
        -- Check a single block under assumptions of a cursor at offs
        checkBlock (offs, l, state) blk = (offs + blockSize blk,
                                           l - blockSize blk,
                                           state && offs == blockOffset blk)
        checkContents os l blks = case foldl checkBlock (os, l, True) blks of
                                    (_, 0, True) -> True
                                    _            -> False
        assertAllDownloaded blocks p = all (\(p', _) -> p /= p') blocks

-- | Update the progress on a Piece. When we get a block from the piece, we will
--   track this in the Piece Database. This function returns @complete@
--   where @complete@ is @True@ if the piece is percieved to be complete and @False@
--   otherwise.
updateProgress :: PieceNum -> Block -> PieceMgrProcess Bool
updateProgress pn blk = do
    ipdb <- gets inProgress
    case M.lookup pn ipdb of
      Nothing -> do warningP "updateProgress can't find progress block, error?"
                    return False
      Just pg ->
          let blkSet = ipHaveBlocks pg
          in if blk `S.member` blkSet
               then return False -- Stray block download.
                                 -- Will happen without FAST extension
                                 -- at times
               else checkComplete pg { ipHaveBlocks = S.insert blk blkSet }
  where checkComplete pg = do
            modify (\db -> db { inProgress = M.adjust (const pg) pn (inProgress db) })
            debugP $ "Iphave : " ++ show (ipHave pg) ++ " ipDone: " ++ show (ipDone pg)
            return (ipHave pg == ipDone pg)
        ipHave = S.size . ipHaveBlocks

blockPiece :: BlockSize -> PieceSize -> [Block]
blockPiece blockSz pieceSize = build pieceSize 0 []
  where build 0        _os accum = reverse accum
        build leftBytes os accum | leftBytes >= blockSz =
                                     build (leftBytes - blockSz)
                                           (os + blockSz)
                                           $ Block os blockSz : accum
                                 | otherwise = build 0 (os + leftBytes) $ Block os leftBytes : accum

-- | The call @grabBlocks n eligible@ tries to pick off up to @n@ pieces from
--   to download. In doing so, it will only consider pieces in @eligible@. It
--   returns a list of Blocks which where grabbed.
grabBlocks :: Int -> PS.PieceSet -> PieceMgrProcess Blocks
grabBlocks k eligible = {-# SCC "grabBlocks" #-} do
    ps' <- PS.copy eligible
    blocks <- tryGrabProgress k ps' []
    pend <- gets pendingPieces
    pendN <- PS.null pend
    if blocks == [] && pendN
        then do ps'' <- PS.copy eligible
                blks <- grabEndGame k ps''
                modify (\db -> db { endGaming = True })
                debugP $ "PieceMgr entered endgame."
                return $ Endgame blks
        else do modify (\s -> s { downloading = blocks ++ (downloading s) })
                return $ Leech blocks

-- Grabbing blocks is a state machine implemented by tail calls
-- Try grabbing pieces from the pieces in progress first
tryGrabProgress :: PieceNum -> PS.PieceSet -> [(PieceNum, Block)]
                -> Process CF ST [(PieceNum, Block)]
tryGrabProgress 0 _  captured = return captured
tryGrabProgress n ps captured = grabber =<< (M.keys <$> gets inProgress)
  where
    grabber []       = tryGrabPending n ps captured
    grabber (i : is) = do m <- PS.member i ps
                          if m
                            then grabFromProgress n ps i captured
                            else grabber is

-- The Piece @p@ was found, grab it
grabFromProgress :: PieceNum -> PS.PieceSet -> PieceNum -> [(PieceNum, Block)]
                 -> Process CF ST [(PieceNum, Block)]
grabFromProgress n ps p captured = do
    inprog <- gets inProgress
    ipp <- case M.lookup p inprog of
              Nothing -> fail "grabFromProgress: could not lookup piece"
              Just x -> return x
    let (grabbed, rest) = splitAt n (ipPendingBlocks ipp)
        nIpp = ipp { ipPendingBlocks = rest }
    -- This rather ugly piece of code should be substituted with something better
    if grabbed == []
         -- All pieces are taken, try the next one.
         then do PS.delete p ps --TODO: Dangerous since we will NEVER reconsider that piece then!
                 tryGrabProgress n ps captured
         else do modify (\db -> db { inProgress = M.insert p nIpp inprog })
                 tryGrabProgress (n - length grabbed) ps ([(p,g) | g <- grabbed] ++ captured)

-- Try grabbing pieces from the pending blocks
tryGrabPending :: PieceNum -> PS.PieceSet -> [(PieceNum, Block)] -> Process CF ST [(PieceNum, Block)]
tryGrabPending n ps captured = do
    histo <- gets histogram
    pending <- gets pendingPieces
    selector <- PS.freeze ps
    pendingS <- PS.freeze pending
    let culprits = PendS.pick (\p -> selector p && pendingS p) histo
    case culprits of
        Nothing -> do
            isn <- PS.intersection ps pending
            assert (null isn) (return ())
            return captured
        Just pieces -> do
            h <- pickRandom pieces
            inProg <- gets inProgress
            blockList <- createBlock h
            let sz  = length blockList
                ipp = InProgressPiece sz S.empty blockList
            PS.delete h =<< gets pendingPieces
            modify (\db -> db { inProgress    = M.insert h ipp inProg })
            tryGrabProgress n ps captured

grabEndGame :: PieceNum -> PS.PieceSet -> Process CF ST [(PieceNum, Block)]
grabEndGame n ps = do -- In endgame we are allowed to grab from the downloaders
    dls <- filterM (\(p, _) -> PS.member p ps) =<< gets downloading
    take n . shuffle' dls (length dls) <$> liftIO newStdGen

-- | Pick a random element among a finite list af them.
pickRandom :: MonadIO m => [a] -> m a
pickRandom ls = do
    n <- liftIO $ getStdRandom (\gen -> randomR (0, length ls - 1) gen)
    return $ ls !! n

-- | If given a Piece number, convert said number into its list of blocks to
-- download at peers.
createBlock :: PieceNum -> PieceMgrProcess [Block]
createBlock pn = do
     gets infoMap >>= (\im -> case M.lookup pn im of
                                 Nothing -> fail "createBlock: could not lookup piece"
                                 Just ipp -> return $ cBlock ipp)
         where cBlock = blockPiece defaultBlockSize . fromInteger . len

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM f l = do r <- mapM f l
              return (any (==True) r)

assertST :: PieceMgrProcess ()
assertST = {-# SCC "assertST" #-} do
    c <- gets assertCount
    if c == 0
        then do modify (\db -> db { assertCount = 10 })
                assertSets >> assertInProgress >> assertDownloading
                sizes <- sizeReport
                debugP sizes
        else modify (\db -> db { assertCount = assertCount db - 1 })
  where
    -- If a piece is pending in the database, we have the following rules:
    --
    --  - It is not done.
    --  - It is not being downloaded
    --  - It is not in progresss.
    --
    -- If a piece is done, we have the following rules:
    --
    --  - It is not in progress.
    --  - There are no more downloading blocks.
    assertSets = do
        pending <- gets pendingPieces
        done    <- gets donePiece
        down    <- return . map fst =<< gets downloading
        iprog   <- return . M.keys  =<< gets inProgress
        pdownis <- anyM (flip PS.member pending) down
        donedownis <- anyM (flip PS.member done) down
        pdis <- PS.intersection pending done
        piprogis <- anyM (flip PS.member pending) iprog
        doneprogis <- anyM (flip PS.member done) iprog

        when (not $ null pdis)
           (do trb <- gets traceBuffer
               liftIO $ print trb
               return $ assert False ())
        when pdownis
           (do trb <- gets traceBuffer
               liftIO $ print trb
               return $ assert False ())
        when piprogis
           (do trb <- gets traceBuffer
               liftIO $ print trb
               return $ assert False ())
        when doneprogis
           (do trb <- gets traceBuffer
               liftIO $ print trb
               return $ assert False ())
        when donedownis
           (do trb <- gets traceBuffer
               liftIO $ print trb
               return $ assert False ())

    -- If a piece is in Progress, we have:
    --
    --  - There is a relationship with what pieces are downloading
    --    - If a block is ipPending, it is not in the downloading list
    --    - If a block is ipHave, it is not in the downloading list
    assertInProgress = do
        inProg <- gets inProgress
        mapM_ checkInProgress $ M.toList inProg
    checkInProgress (pn, ipp) = do
        when ( (S.size $ ipHaveBlocks ipp) >= ipDone ipp)
            (fail $ "Piece in progress " ++ show pn
                    ++ " has downloaded more blocks than the piece has")
    assertDownloading = do
        down <- gets downloading
        mapM_ checkDownloading down
    checkDownloading (pn, blk) = do
        prog <- gets inProgress
        tr   <- gets traceBuffer
        case M.lookup pn prog of
            Nothing -> fail $ "Piece " ++ show pn ++ " not in progress while We think it was"
            Just ipp -> do
                when (blk `elem` ipPendingBlocks ipp)
                    (fail $ "P/Blk " ++ show (pn, blk) ++ " is in the Pending Block list")
                when (S.member blk $ ipHaveBlocks ipp)
                    (fail $ "P/Blk " ++ show (pn, blk) ++ " is in the HaveBlocks set" ++
                        "Trace: " ++ show tr)

