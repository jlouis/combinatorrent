module PieceMgrP
where

import Data.List

import ConsoleP
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
    { pendingPiece :: [PieceNum] -- ^ Pieces currently pending download
    , donePiece    :: [PieceNum] -- ^ Pieces that are done
    , inProgress   :: [InProgressPiece] -- ^ Pieces in progress
    }

-- | The InProgressPiece data type describes pieces in progress of being downloaded.
--   we keep track of blocks which are pending as well as blocks which are done. We
--   also keep track of a count of the blocks. When a block is done, we cons it unto
--   @ipHaveBlocks@. When @ipHave == ipDone@, we check the piece for correctness. The
--   field @ipHaveBlocks@ could in principle be omitted, but for now it is kept since
--   we can use it for asserting implementation correctness. We note that both the
--   check operations are then O(1) and probably fairly fast.
data InProgressPiece = InProgressPiece
    { ipPiece :: PieceNum -- ^ PieceNum which is in Progress
    , ipHave  :: Int -- ^ Number of blocks we have
    , ipDone  :: Int -- ^ Number of blocks done
    , ipHaveBlocks :: [Block] -- ^ The blocks we have
    , ipPendingBlocks :: [Block] -- ^ Blocks still pending
    } deriving Show

----------------------------------------------------------------------

start :: LogChannel -> IO ()
start = undefined

----------------------------------------------------------------------

blockPiece :: BlockSize -> PieceSize -> [Block]
blockPiece blockSz pieceSize = build pieceSize 0 []
  where build leftBytes os accum | leftBytes >= blockSz =
                                     build (leftBytes - blockSz)
                                           (os + blockSz)
                                           $ (Block os blockSz) : accum
                                 | otherwise = reverse $ Block os leftBytes : accum


-- | The call @grabBlocks n eligible db@ tries to pick off up to @n@ pieces from
--   the @n@. In doing so, it will only consider pieces in @eligible@. It returns a
--   pair @(blocks, db')@, where @blocks@ are the blocks it picked and @db'@ is the resulting
--   db with these blocks removed.
grabBlocks :: Int -> [PieceNum] -> PieceDB -> ([(PieceNum, [Block])], PieceDB)
grabBlocks = undefined

-- | Put back a set of blocks into the piece database. This call is used whenever we loose a
--   peer as his share of blocks are to be added back for downloading.
putBlock :: [(PieceNum, [Block])] -> PieceDB -> PieceDB
putBlock = undefined

-- | Predicate function. Is the block list a complete blocklist at the moment? The call @completePiece sz blks@
--   will answer this question for the case where the piece is of size @sz@. As a precondition we consider @blks@
--   to be sorted by offset
completePiece :: Int -> Int -> [Block] -> Bool
completePiece = undefined
-- Blocks are considered sorted.
--   We can thus just walk through the blocks and sum them, finally checking if the sum reaches the goal.

