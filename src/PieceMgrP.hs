module PieceMgrP
    ( PieceMgrMsg(..)
    , PieceMgrChannel
    , ChokeInfoChannel
    , ChokeInfoMsg(..)
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

import Logging
import FSP hiding (start, fspCh)
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
    , donePush      :: [PieceNum] -- ^ Pieces that should be pushed to the Choke Mgr.
    , inProgress    :: M.Map PieceNum InProgressPiece -- ^ Pieces in progress
    , infoMap       :: PieceMap   -- ^ Information about pieces
    }

-- | The InProgressPiece data type describes pieces in progress of being downloaded.
--   we keep track of blocks which are pending as well as blocks which are done. We
--   also keep track of a count of the blocks. When a block is done, we cons it unto
--   @ipHaveBlocks@. When @ipHave == ipDone@, we check the piece for correctness. The
--   field @ipHaveBlocks@ could in principle be omitted, but for now it is kept since
--   we can use it for asserting implementation correctness. We note that both the
--   check operations are then O(1) and probably fairly fast.
data InProgressPiece = InProgressPiece
    { ipDone  :: Int -- ^ Number of blocks when piece is done
    , ipSize  :: Int -- ^ Size of the Piece we are downloading
    , ipHaveBlocks :: S.Set Block -- ^ The blocks we have
    , ipPendingBlocks :: [Block] -- ^ Blocks still pending
    } deriving Show

-- INTERFACE
----------------------------------------------------------------------

data PieceMgrMsg = GrabBlocks Int [PieceNum] (Channel [(PieceNum, [Block])])
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
                  | TorrentComplete

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
  getLogger = logCh

type PieceMgrProcess v = Process PieceMgrCfg PieceDB v

start :: LogChannel -> PieceMgrChannel -> FSPChannel -> ChokeInfoChannel -> StatusChan -> PieceDB
      -> SupervisorChan -> IO ThreadId
start logC mgrC fspC chokeC statC db supC =
    spawnP (PieceMgrCfg logC mgrC fspC chokeC statC) db
		    (catchP (forever pgm)
			(defaultStopHandler supC))
  where pgm = do
	  dl <- gets donePush
	  (if dl == []
	      then receiveEvt
	      else chooseP [receiveEvt, sendEvt (head dl)]) >>= syncP
	sendEvt elem = do
	    ev <- sendPC chokeCh (PieceDone elem)
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
		    do storeBlock pn blk d
		       done <- updateProgress pn blk
		       when done
			   (do assertPieceComplete pn
			       pm <- gets infoMap
			       let l = len $ fromJust $ M.lookup pn pm
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
	markDone pn = do
	    modify (\db -> db { donePush = pn : donePush db })
	checkPiece n = do
	    ch <- liftIO channel
	    syncP =<< (sendPC fspCh $ CheckPiece n ch)
	    syncP =<< recvP ch (const True)

-- HELPERS
----------------------------------------------------------------------

createPieceDb :: PiecesDoneMap -> PieceMap -> PieceDB
createPieceDb mmap pmap = PieceDB pending done [] M.empty pmap
  where pending = M.keys $ M.filter (==False) mmap
        done    = M.keys $ M.filter (==True) mmap

----------------------------------------------------------------------

-- | The call @completePiece db pn@ will mark that the piece @pn@ is completed
--   and return the updated Piece Database.
completePiece :: PieceNum -> PieceMgrProcess ()
completePiece pn = modify (\db -> db { inProgress = M.delete pn (inProgress db),
                                       donePiece  = pn : donePiece db })

checkFullCompletion :: PieceMgrProcess ()
checkFullCompletion = do
    done <- gets pendingPieces
    ipp  <- gets inProgress
    when (done == [] && M.null ipp)
	(do logInfo "Torrent Completed"
	    sendPC statusCh STP.TorrentCompleted >>= syncP
	    sendPC chokeCh  TorrentComplete >>= syncP)

-- | The call @putBackPiece db pn@ will mark the piece @pn@ as not being complete
--   and put it back into the download queue again. Returns the new database.
putbackPiece :: PieceNum -> PieceMgrProcess ()
putbackPiece pn = modify (\db -> db { inProgress = M.delete pn (inProgress db),
                                      pendingPieces = pn : pendingPieces db })

putbackBlock :: (PieceNum, Block) -> PieceMgrProcess ()
putbackBlock (pn, blk) = modify (\db -> db { inProgress = ndb (inProgress db)})
  where ndb db = M.alter f pn db
        -- The first of these might happen in the endgame
        f Nothing     = error "The 'impossible' happened, are you implementing endgame?"
        f (Just ipp) = Just ipp { ipPendingBlocks = blk : ipPendingBlocks ipp }

-- | Assert that a Piece is Complete. Can be omitted when we know it works
--   and we want a faster client.
assertPieceComplete :: PieceNum -> PieceMgrProcess ()
assertPieceComplete pn = do
    inprog <- gets inProgress
    let ipp = fromJust $ M.lookup pn inprog
    unless (assertComplete ipp)
      (do logError $ "Could not assert completion of the piece with block state " ++ show ipp
	  stopP)
  where assertComplete ip = checkContents 0 (ipSize ip) (S.toAscList (ipHaveBlocks ip))
        -- Check a single block under assumptions of a cursor at offs
        checkBlock (offs, left, state) blk = (offs + blockSize blk,
                                              left - blockSize blk,
                                              state && offs == blockOffset blk)
        checkContents os l blks = case foldl checkBlock (os, l, True) blks of
                                    (_, 0, True) -> True
                                    _            -> False

-- | Update the progress on a Piece. When we get a block from the piece, we will
--   track this in the Piece Database. This function returns a pair @(complete, nDb)@
--   where @complete@ is @True@ if the piece is percieved to be complete and @False@
--   otherwise. @nDb@ is the updated Piece Database
updateProgress :: PieceNum -> Block -> PieceMgrProcess Bool
updateProgress pn blk = do
    ipdb <- gets inProgress
    case M.lookup pn ipdb of
      Nothing -> return False -- XXX: Ignore, this might be wrong
      Just pg ->
          let blkSet = ipHaveBlocks pg
          in if blk `S.member` blkSet
               then return False -- Stray block download.
                                 -- Will happen without FAST extension
                                 -- at times
               else checkComplete pg { ipHaveBlocks = S.insert blk blkSet }
  where checkComplete pg = do
	    modify (\db -> db { inProgress = M.adjust (const pg) pn (inProgress db) })
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
grabBlocks' :: Int -> [PieceNum] -> PieceMgrProcess [(PieceNum, [Block])]
grabBlocks' k eligible = tryGrabProgress k eligible []
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
        let ipp = fromJust $ M.lookup p inprog
            (grabbed, rest) = splitAt n (ipPendingBlocks ipp)
            nIpp = ipp { ipPendingBlocks = rest }
        -- This rather ugly piece of code should be substituted with something better
        if grabbed == []
             -- All pieces are taken, try the next one.
             then tryGrabProgress n (ps \\ [p]) captured
             else do modify (\db -> db { inProgress = M.insert p nIpp inprog })
		     tryGrabProgress (n - length grabbed) ps ((p, grabbed) : captured)
    -- Try grabbing pieces from the pending blocks
    tryGrabPending n ps captured = do
	pending <- gets pendingPieces
        case ps `intersect` pending of
          []    -> return captured -- No (more) pieces to download, return
          ls    -> do
	      h <- pickRandom ls
	      infMap <- gets infoMap
	      inProg <- gets inProgress
              blockList <- createBlock h
              let bSz = fromInteger . len $ fromJust $ M.lookup n infMap
	          ipp = InProgressPiece 0 bSz S.empty blockList
              modify (\db -> db { pendingPieces = pendingPieces db \\ [h],
                                  inProgress    = M.insert h ipp inProg })
	      tryGrabProgress n ps captured
    pickRandom pieces = do
	n <- liftIO $ getStdRandom (\gen -> randomR (0, length pieces - 1) gen)
	return $ pieces !! n
    createBlock :: Int -> PieceMgrProcess [Block]
    createBlock pn =
	let cBlock = blockPiece
                        defaultBlockSize . fromInteger . len . fromJust . M.lookup pn . infoMap
	in
	    get >>= return . cBlock



