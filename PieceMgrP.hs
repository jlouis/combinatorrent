module PieceMgrP
where

import Data.List

import ConsoleP
import Torrent

start :: LogChannel -> IO ()
start = undefined


----------------------------------------------------------------------

data PieceDB = PieceDB { pendingPiece :: [PieceNum] -- ^ Pieces currently pending download
                       , inProgressPiece  :: [(PieceNum, [Block])] -- ^ Pieces in progress of being downloaded
                       , donePiece :: [PieceNum] -- ^ Pieces that are done
                       }

blockPiece :: BlockSize -> PieceSize -> [Block]
blockPiece blockSz pieceSize = build pieceSize 0 []
  where build leftBytes os accum | leftBytes >= blockSz = build (leftBytes - blockSz)
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


-- | Mark a Piece as done
pieceDone :: PieceNum -> PieceDB -> PieceDB
pieceDone pn db = db { donePiece = pn : (donePiece db),
                       inProgressPiece = ipp }
  where ipp = deleteBy eq (pn, []) (inProgressPiece db)
        eq (x, _) (y, _) = x == y


