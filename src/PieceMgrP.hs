module PieceMgrP
where

import Control.Concurrent.CML

import qualified Data.ByteString as B
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import ConsoleP
import FSP
import Torrent

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
                 | GetDone (Channel [PieceNum])

type PieceMgrChannel = Channel PieceMgrMsg

start :: LogChannel -> PieceMgrChannel -> FSPChannel -> PieceDB -> IO ()
start logC mgrC fspC db = (spawn $ lp db) >> return ()
  where lp db = do
          msg <- sync $ receive mgrC (const True)
          case msg of
            GrabBlocks n eligible c ->
                do logMsg logC $ "Grabbing blocks"
                   let (blocks, db') = grabBlocks' n eligible db
                   logMsg logC $ "Grabbed..."
                   sync $ transmit c blocks
                   lp db'
            StoreBlock pn blk d ->
                do FSP.storeBlock fspC pn blk d
                   let (done, db') = updateProgress db pn blk
                   if done
                      then do assertPieceComplete db pn logC
                              pieceOk <- FSP.checkPiece fspC pn
                              let db'' = case pieceOk of
                                           Nothing ->
                                               error "PieceMgrP: Piece Nonexisting!"
                                           Just True -> completePiece db' pn
                                           Just False -> putBackPiece db' pn
                              lp db''
                      else lp db'
            PutbackBlocks blks ->
              lp $ foldl (\db (pn, blk) -> putbackBlock pn blk db) db blks
            GetDone c -> do sync $ transmit c (donePiece db)
                            lp db

getPieceDone :: PieceMgrChannel -> IO [PieceNum]
getPieceDone ch = do
  c <- channel
  sync $ transmit ch $ GetDone c
  sync $ receive c (const True)

putbackBlocks :: PieceMgrChannel -> [(PieceNum, Block)] -> IO ()
putbackBlocks ch blks = sync $ transmit ch (PutbackBlocks blks)

storeBlock :: PieceMgrChannel -> PieceNum -> Block -> B.ByteString -> IO ()
storeBlock ch n blk bs = sync $ transmit ch (StoreBlock n blk bs)

grabBlocks :: PieceMgrChannel -> Int -> [PieceNum] -> IO [(PieceNum, Block)]
grabBlocks pmC n pieceSet = do
    c <- channel :: IO (Channel ([(PieceNum, [Block])]))
    sync $ transmit pmC (GrabBlocks n pieceSet c)
    blks <- sync $ receive c (const True)
    return [(pn, b) | (pn, blklst) <- blks, b <- blklst]

-- HELPERS
----------------------------------------------------------------------

createPieceDb :: PiecesDoneMap -> PieceMap -> PieceDB
createPieceDb mmap pmap = PieceDB pending done M.empty pmap
  where pending = M.keys $ M.filter (==False) mmap
        done    = M.keys $ M.filter (==True) mmap

----------------------------------------------------------------------

-- | The call @completePiece db pn@ will mark that the piece @pn@ is completed
--   and return the updated Piece Database.
completePiece :: PieceDB -> PieceNum -> PieceDB
completePiece db pn =
    db { inProgress = M.delete pn (inProgress db),
         donePiece  = pn : donePiece db }

-- | The call @putBackPiece db pn@ will mark the piece @pn@ as not being complete
--   and put it back into the download queue again. Returns the new database.
putBackPiece :: PieceDB -> PieceNum -> PieceDB
putBackPiece db pn =
    db { inProgress = M.delete pn (inProgress db),
         pendingPieces = pn : pendingPieces db }

putbackBlock :: PieceNum -> Block -> PieceDB -> PieceDB
putbackBlock pn blk db = db { inProgress = ndb }
  where ndb = M.alter f pn (inProgress db)
        f Nothing     = error "The 'impossible' happened, are you implementing endgame?" -- This might happen in endgame
        f (Just ipp) = Just ipp { ipPendingBlocks = blk : ipPendingBlocks ipp }

-- | Assert that a Piece is Complete. Can be omitted when we know it works
--   and we want a faster client.
assertPieceComplete :: PieceDB -> PieceNum -> LogChannel -> IO ()
assertPieceComplete db pn logC = do
    let ipp = fromJust $ M.lookup pn (inProgress db)
    if assertComplete ipp
      then return ()
      else do logFatal logC $ "Could not assert completion of the piece with block state " ++ show ipp
              return ()
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
updateProgress :: PieceDB -> PieceNum -> Block -> (Bool, PieceDB)
updateProgress db pn blk =
    case M.lookup pn ipdb of
      Nothing -> (False, db) -- Ignore, this might be wrong
      Just pg ->
          let blkSet = ipHaveBlocks pg
          in if blk `S.member` blkSet
               then (False, db) -- Stray block download.
                                -- Will happen without FAST extension
                                -- at times
               else checkComplete pg { ipHaveBlocks = S.insert blk blkSet }
  where checkComplete pg = (ipHave pg == ipDone pg, db { inProgress =
                                                             M.adjust (const pg) pn ipdb})
        ipHave pg = S.size (ipHaveBlocks pg)
        ipdb = inProgress db

blockPiece :: BlockSize -> PieceSize -> [Block]
blockPiece blockSz pieceSize = build pieceSize 0 []
  where build leftBytes os accum | leftBytes >= blockSz =
                                     build (leftBytes - blockSz)
                                           (os + blockSz)
                                           $ (Block os blockSz) : accum
                                 | otherwise = reverse $ Block os leftBytes : accum


-- | The call @grabBlocks' n eligible db@ tries to pick off up to @n@ pieces from
--   the @n@. In doing so, it will only consider pieces in @eligible@. It returns a
--   pair @(blocks, db')@, where @blocks@ are the blocks it picked and @db'@ is the resulting
--   db with these blocks removed.
grabBlocks' :: Int -> [PieceNum] -> PieceDB -> ([(PieceNum, [Block])], PieceDB)
grabBlocks' k eligible db = tryGrabProgress k eligible db []
  where
    -- Grabbing blocks is a state machine implemented by tail calls
    -- Try grabbing pieces from the pieces in progress first
    tryGrabProgress 0 _  db captured = (captured, db)
    tryGrabProgress n ps db captured =
        case ps `intersect` (fmap fst $ M.toList (inProgress db)) of
          []  -> tryGrabPending n ps db captured
          (h:_) -> grabFromProgress n ps h db captured
    -- The Piece @p@ was found, grab it
    grabFromProgress n ps p db captured =
        let ipp = fromJust $ M.lookup p (inProgress db)
            (grabbed, rest) = splitAt n (ipPendingBlocks ipp)
            nIpp = ipp { ipPendingBlocks = rest }
            nDb  = db { inProgress = M.insert p nIpp (inProgress db) }
        in
          tryGrabProgress (n - length grabbed) ps nDb ((p, grabbed) : captured)
    -- Try grabbing pieces from the pending blocks
    tryGrabPending n ps db captured =
        case ps `intersect` (pendingPieces db) of
          []    -> (captured, db) -- No (more) pieces to download, return
          (h:_) ->
              let blockList = createBlock n db
                  ipp = InProgressPiece 0 (length blockList) S.empty blockList
                  nDb = db { pendingPieces = (pendingPieces db) \\ [h],
                             inProgress    = M.insert h ipp (inProgress db) }
              in tryGrabProgress n ps nDb captured
    createBlock :: Int -> PieceDB -> [Block]
    createBlock n pdb = blockPiece
                          defaultBlockSize
                          (len
                           (fromJust $ M.lookup n (infoMap pdb)))


