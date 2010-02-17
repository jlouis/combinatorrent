module PieceMgrP
    ( PieceMgrMsg(..)
    , PieceMgrChannel
    , ChokeInfoChannel
    , ChokeInfoMsg(..)
    , Blocks(..)
    , start
    , createPieceDb
    )
where


import Control.Concurrent
import Control.Concurrent.CML
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as S

import Prelude hiding (log)

import System.Random
import System.Random.Shuffle

import Logging
import FSP hiding (start)
import StatusP as STP hiding (start) 
import Supervisor
import Torrent
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
data PieceDB = PieceDB
    { pendingPieces :: [PieceNum] -- ^ Pieces currently pending download
    , donePiece     :: [PieceNum] -- ^ Pieces that are done
    , donePush      :: [ChokeInfoMsg] -- ^ Pieces that should be pushed to the Choke Mgr.
    , inProgress    :: M.Map PieceNum InProgressPiece -- ^ Pieces in progress
    , downloading   :: [(PieceNum, Block)]    -- ^ Blocks we are currently downloading
    , infoMap       :: PieceMap   -- ^ Information about pieces
    , endGaming     :: Bool       -- ^ If we have done any endgame work this is true
    } deriving Show

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
data PieceMgrMsg = GrabBlocks Int [PieceNum] (Channel Blocks)
                   -- ^ Ask for grabbing some blocks
                 | StoreBlock PieceNum Block B.ByteString
                   -- ^ Ask for storing a block on the file system
                 | PutbackBlocks [(PieceNum, Block)]
                   -- ^ Put these blocks back for retrieval
		 | AskInterested [PieceNum] (Channel Bool)
		   -- ^ Ask if any of these pieces are interesting
                 | GetDone (Channel [PieceNum])
		   -- ^ Get the pieces which are already done

data ChokeInfoMsg = PieceDone PieceNum
		  | BlockComplete PieceNum Block
                  | TorrentComplete
    deriving (Eq, Show)

type PieceMgrChannel = Channel PieceMgrMsg
type ChokeInfoChannel = Channel ChokeInfoMsg

data PieceMgrCfg = PieceMgrCfg
    { logCh :: LogChannel
    , pieceMgrCh :: PieceMgrChannel
    , fspCh :: FSPChannel
    , chokeCh :: ChokeInfoChannel
    , statusCh :: StatusChan
    }

instance Logging PieceMgrCfg where
  getLogger cf = ("PieceMgrP", logCh cf)

type PieceMgrProcess v = Process PieceMgrCfg PieceDB v

start :: LogChannel -> PieceMgrChannel -> FSPChannel -> ChokeInfoChannel -> StatusChan -> PieceDB
      -> SupervisorChan -> IO ThreadId
start logC mgrC fspC chokeC statC db supC =
    spawnP (PieceMgrCfg logC mgrC fspC chokeC statC) db
		    (catchP (forever pgm)
			(defaultStopHandler supC))
  where pgm = do
	  assertPieceDB
	  dl <- gets donePush
	  (if dl == []
	      then receiveEvt
	      else chooseP [receiveEvt, sendEvt (head dl)]) >>= syncP
	sendEvt elem = do
	    ev <- sendPC chokeCh elem
	    wrapP ev remDone
	remDone :: () -> Process PieceMgrCfg PieceDB ()
	remDone () = modify (\db -> db { donePush = tail (donePush db) })
        receiveEvt = do
	    ev <- recvPC pieceMgrCh
	    wrapP ev (\msg ->
	      case msg of
		GrabBlocks n eligible c ->
		    do logDebug "Grabbing Blocks"
		       blocks <- grabBlocks' n eligible
		       logDebug "Grabbed..."
		       syncP =<< sendP c blocks
		StoreBlock pn blk d ->
		    do logDebug $ "Storing block: " ++ show (pn, blk)
		       storeBlock pn blk d
		       modify (\s -> s { downloading = downloading s \\ [(pn, blk)] })
		       endgameBroadcast pn blk
		       done <- updateProgress pn blk
		       when done
			   (do assertPieceComplete pn
			       pend <- gets pendingPieces
			       iprog <- gets inProgress
			       logInfo $ "Piece #" ++ show pn
					 ++ " completed, there are " 
					 ++ (show $ length pend) ++ " pending "
					 ++ (show $ M.size iprog) ++ " in progress"
			       l <- gets infoMap >>=
				    (\pm -> case M.lookup pn pm of
						    Nothing -> fail "Storeblock: M.lookup"
						    Just x -> return $ len x)
			       sendPC statusCh (CompletedPiece l) >>= syncP
			       pieceOk <- checkPiece pn
			       case pieceOk of
				 Nothing ->
					do logFatal "PieceMgrP: Piece Nonexisting!"
					   stopP
				 Just True -> do completePiece pn
						 markDone pn
						 checkFullCompletion
				 Just False -> putbackPiece pn)
		PutbackBlocks blks ->
		    mapM_ putbackBlock blks
		GetDone c -> do done <- gets donePiece
				syncP =<< sendP c done
		AskInterested pieces retC -> do
		    inProg <- liftM (S.fromList . M.keys) $ gets inProgress
		    pend   <- liftM S.fromList $ gets pendingPieces
		    -- @i@ is the intersection with with we need and the peer has.
		    let i = S.null $ S.intersection (S.fromList pieces)
		                   $ S.union inProg pend 
		    syncP =<< sendP retC (not i))
	storeBlock n blk contents = syncP =<< (sendPC fspCh $ WriteBlock n blk contents)
	endgameBroadcast pn blk =
	    gets endGaming >>=
	      flip when (modify (\db -> db { donePush = (BlockComplete pn blk) : donePush db }))
	markDone pn = do
	    modify (\db -> db { donePush = (PieceDone pn) : donePush db })
	checkPiece n = do
	    ch <- liftIO channel
	    syncP =<< (sendPC fspCh $ CheckPiece n ch)
	    syncP =<< recvP ch (const True)

-- HELPERS
----------------------------------------------------------------------

createPieceDb :: PiecesDoneMap -> PieceMap -> PieceDB
createPieceDb mmap pmap = PieceDB pending done [] M.empty [] pmap False
  where pending = M.keys $ M.filter (==False) mmap
        done    = M.keys $ M.filter (==True) mmap

----------------------------------------------------------------------

-- | The call @completePiece db pn@ will mark that the piece @pn@ is completed
--   and return the updated Piece Database.
completePiece :: PieceNum -> PieceMgrProcess ()
completePiece pn = modify (\db -> db { inProgress = M.delete pn (inProgress db),
                                       donePiece  = pn : donePiece db })

-- | Handle torrent completion
checkFullCompletion :: PieceMgrProcess ()
checkFullCompletion = do
    doneP <- gets donePiece
    im    <- gets infoMap
    when (M.size im == length doneP)
	(do logInfo "Torrent Completed"
	    sendPC statusCh STP.TorrentCompleted >>= syncP
	    sendPC chokeCh  TorrentComplete >>= syncP)

-- | The call @putBackPiece db pn@ will mark the piece @pn@ as not being complete
--   and put it back into the download queue again. Returns the new database.
putbackPiece :: PieceNum -> PieceMgrProcess ()
putbackPiece pn = modify (\db -> db { inProgress = M.delete pn (inProgress db),
                                      pendingPieces = pn : pendingPieces db })

-- | Put back a block for downloading.
--   TODO: This is rather slow, due to the (\\) call, but hopefully happens rarely.
putbackBlock :: (PieceNum, Block) -> PieceMgrProcess ()
putbackBlock (pn, blk) = modify (\db -> db { inProgress = ndb (inProgress db)
					   , downloading = downloading db \\ [(pn, blk)]})
  where ndb db = M.alter f pn db
        -- The first of these might happen in the endgame
        f Nothing     = error "The 'impossible' happened, are you implementing endgame?"
        f (Just ipp) = Just ipp { ipPendingBlocks = blk : ipPendingBlocks ipp }

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
        checkBlock (offs, left, state) blk = (offs + blockSize blk,
                                              left - blockSize blk,
                                              state && offs == blockOffset blk)
        checkContents os l blks = case foldl checkBlock (os, l, True) blks of
                                    (_, 0, True) -> True
                                    _            -> False
	assertAllDownloaded blocks pn = all (\(pn', _) -> pn /= pn') blocks

-- | Update the progress on a Piece. When we get a block from the piece, we will
--   track this in the Piece Database. This function returns a pair @(complete, nDb)@
--   where @complete@ is @True@ if the piece is percieved to be complete and @False@
--   otherwise.
updateProgress :: PieceNum -> Block -> PieceMgrProcess Bool
updateProgress pn blk = do
    ipdb <- gets inProgress
    case M.lookup pn ipdb of
      Nothing -> do logDebug "updateProgress can't find progress block, error?"
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
	    logDebug $ "Iphave : " ++ show (ipHave pg) ++ " ipDone: " ++ show (ipDone pg)
	    return (ipHave pg == ipDone pg)
        ipHave = S.size . ipHaveBlocks

blockPiece :: BlockSize -> PieceSize -> [Block]
blockPiece blockSz pieceSize = build pieceSize 0 []
  where build 0         os accum = reverse accum
        build leftBytes os accum | leftBytes >= blockSz =
                                     build (leftBytes - blockSz)
                                           (os + blockSz)
                                           $ Block os blockSz : accum
                                 | otherwise = build 0 (os + leftBytes) $ Block os leftBytes : accum

-- | The call @grabBlocks' n eligible db@ tries to pick off up to @n@ pieces from
--   the @n@. In doing so, it will only consider pieces in @eligible@. It returns a
--   pair @(blocks, db')@, where @blocks@ are the blocks it picked and @db'@ is the resulting
--   db with these blocks removed.
grabBlocks' :: Int -> [PieceNum] -> PieceMgrProcess Blocks
grabBlocks' k eligible = do
    blocks <- tryGrabProgress k eligible []
    pend <- gets pendingPieces
    if blocks == [] && pend == []
	then do blks <- grabEndGame k (S.fromList eligible)
		modify (\db -> db { endGaming = True })
		logDebug $ "PieceMgr entered endgame."
		return $ Endgame blks
	else do modify (\s -> s { downloading = blocks ++ (downloading s) })
		return $ Leech blocks
  where
    -- Grabbing blocks is a state machine implemented by tail calls
    -- Try grabbing pieces from the pieces in progress first
    tryGrabProgress 0 _  captured = return captured
    tryGrabProgress n ps captured = do
	inprog <- gets inProgress
        case ps `intersect` fmap fst (M.toList inprog) of
          []  -> tryGrabPending n ps captured
          (h:_) -> grabFromProgress n ps h captured
    -- The Piece @p@ was found, grab it
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
             then tryGrabProgress n (ps \\ [p]) captured
             else do modify (\db -> db { inProgress = M.insert p nIpp inprog })
		     tryGrabProgress (n - length grabbed) ps ([(p,g) | g <- grabbed] ++ captured)
    -- Try grabbing pieces from the pending blocks
    tryGrabPending n ps captured = do
	pending <- gets pendingPieces
        case ps `intersect` pending of
          []    -> return $ captured -- No (more) pieces to download, return
          ls    -> do
	      h <- pickRandom ls
	      infMap <- gets infoMap
	      inProg <- gets inProgress
              blockList <- createBlock h
              let sz  = length blockList
	          ipp = InProgressPiece sz S.empty blockList
              modify (\db -> db { pendingPieces = pendingPieces db \\ [h],
                                  inProgress    = M.insert h ipp inProg })
	      tryGrabProgress n ps captured
    grabEndGame n ps = do -- In endgame we are allowed to grab from the downloaders
	dls <- liftM (filter (\(p, _) -> S.member p ps)) $ gets downloading
	g <- liftIO newStdGen
        let shuffled = shuffle' dls (length dls) g
	return $ take n shuffled
    pickRandom pieces = do
	n <- liftIO $ getStdRandom (\gen -> randomR (0, length pieces - 1) gen)
	return $ pieces !! n
    createBlock :: Int -> PieceMgrProcess [Block]
    createBlock pn = do
	gets infoMap >>= (\im -> case M.lookup pn im of
				    Nothing -> fail "createBlock: could not lookup piece"
				    Just ipp -> return $ cBlock ipp)
            where cBlock = blockPiece defaultBlockSize . fromInteger . len

assertPieceDB :: PieceMgrProcess ()
assertPieceDB = assertPending >> assertDone >> assertInProgress >> assertDownloading
  where
    -- If a piece is pending in the database, we have the following rules:
    --
    --  - It is not finished.
    --  - It is not being downloaded
    --  - It is not in progresss.
    assertPending = do
	pending <- gets pendingPieces
	mapM_ checkPending pending
    checkPending pn = do
	done <- gets donePiece
	when (pn `elem` done)
	    (fail $ "Pending piece " ++ show pn ++ " is in the done list")
	down <- gets downloading
	when (pn `elem` map fst down)
	    (fail $ "Pending piece " ++ show pn ++ " is in the downloading list")
	inProg <- gets inProgress
	when (case M.lookup pn inProg of
		Nothing -> False
		Just _  -> True)
	    (fail $ "Pending piece " ++ show pn ++ " is in the progress map")
    -- If a piece is done, we have the following rules:
    --
    --  - It is not pending.
    --  - It is not in progress.
    --  - There are no more downloading blocks.
    assertDone    = do
	done <- gets donePiece
	mapM_  checkDone done
    checkDone pn = do
	pending <- gets pendingPieces
	when (pn `elem` pending)
	    (fail $ "Done piece " ++ show pn ++ " is in the pending list")
	down <- gets downloading
	when (pn `elem` map fst down)
	    (fail $ "Done piece " ++ show pn ++ " is in the downloading list")
	inProg <- gets inProgress
	when (case M.lookup pn inProg of 
		Nothing -> False
		Just _  -> True)
	    (fail $ "Done piece " ++ show pn ++ " is in the progress map")
    -- If a piece is in Progress, we have:
    --
    --  - The piece is not Done
    --  - The piece is not pending
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
	done <- gets donePiece
	when (pn `elem` done)
	    (fail $ "Piece in progress " ++ show pn ++ " is in the done list")
	pending <- gets pendingPieces
	when (pn `elem` pending)
	    (fail $ "Piece in progress " ++ show pn ++ " is in the pending list")
    assertDownloading = do
	down <- gets downloading
	mapM_ checkDownloading down
    checkDownloading (pn, blk) = do
	prog <- gets inProgress
	case M.lookup pn prog of
	    Nothing -> fail $ "Piece " ++ show pn ++ " not in progress while We think it was"
	    Just ipp -> do
		when (blk `elem` ipPendingBlocks ipp)
		    (fail $ "P/Blk " ++ show (pn, blk) ++ " is in the Pending Block list")
		when (S.member blk $ ipHaveBlocks ipp)
		    (fail $ "P/Blk " ++ show (pn, blk) ++ " is in the HaveBlocks set")


