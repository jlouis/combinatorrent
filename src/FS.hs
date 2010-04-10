-- | Filesystem routines. These are used for working with and
--   manipulating files in the filesystem.
module FS (PieceInfo(..),
           PieceMap,
           Handles,
           readPiece,
           readBlock,
           writeBlock,
           mkPieceMap,
           checkFile,
           checkPiece,
           openAndCheckFile,
           canSeed)
where

import Control.Monad.State

import Data.Array
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Word

import Foreign.Ptr
import Foreign.ForeignPtr

import System.IO.MMap
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath)

import Protocol.BCode as BCode
import qualified Digest as D
import Torrent

-- | For multi-file torrents we've got to maintain multiple file
--   handles. The data structure may as well be a Map Range Handle,
--   but that's detailto only @projectHandles@. More importantly,
--   functions operating on the files must be aware that a
--   piece/block can span multiple files.
--
--   FIXME: Replace this with a handle cache later. Many peers & many
--          tiny files will make us overstep the fd limit (usually
--          1024).
data MMapF = MMapF {
          regionPtr    :: ForeignPtr Word8
        , regionOffset :: Int
        , regionSize   :: Int
        }

newtype Handles = Handles [MMapF] -- ^ A list of memory mapped regions

projectHandles :: Handles
               -> Int    -- ^Torrent offset
               -> Int    -- ^Torrent size
               -> [(ForeignPtr Word8 -- ^File map location
                   ,Int  -- ^File chunk offset
                   ,Int  -- ^File chunk size
                   )]
projectHandles (Handles []) _ _ = fail "FS: Attempt to read beyond torrent length"
projectHandles (Handles (h1 : handles')) offs size
    | size <= 0 = fail "FS: Should have already stopped projection"
    | offs >= fromIntegral (regionSize h1) =
                projectHandles (Handles handles') (offs - (fromIntegral $ regionSize h1)) size
    | otherwise =
        let size1 = fromIntegral (regionSize h1) - offs  -- ^How much of h1 to take?
        in if size1 >= size
           then [(regionPtr h1, fromIntegral offs + regionOffset h1, size)]
           else  (regionPtr h1, fromIntegral offs + regionOffset h1, size1) :
                projectHandles (Handles handles') 0 (size - size1)

pInfoLookup :: PieceNum -> PieceMap -> IO PieceInfo
pInfoLookup pn mp = return $ mp ! pn

-- | FIXME: minor code duplication with @readBlock@
readPiece :: PieceNum -> Handles -> PieceMap -> IO L.ByteString
readPiece pn handles mp =
    {-# SCC "readPiece" #-}
    do pInfo <- pInfoLookup pn mp
       bs <- return . L.fromChunks $ map (\(p, off, sz) -> BI.fromForeignPtr p off sz)
                          $ projectHandles handles (fromIntegral . offset $ pInfo)
                                                   (fromIntegral . len $ pInfo)
       if L.length bs == (fromInteger . len $ pInfo)
          then return bs
          else fail "FS: Wrong number of bytes read"

-- | FIXME: concatenating strict ByteStrings may turn out
--   expensive. Returning lazy ones may be more appropriate.
readBlock :: PieceNum -> Block -> Handles -> PieceMap -> IO B.ByteString
readBlock pn blk handles mp =
    {-# SCC "readBlock" #-}
    do pInfo <- pInfoLookup pn mp
       return . B.concat $ map (\(p, off, sz) -> BI.fromForeignPtr p off sz)
                $ projectHandles handles (fromIntegral (offset pInfo) + (fromIntegral $ blockOffset blk))
                                         (fromIntegral $ blockSize blk)

-- | The call @writeBlock h n blk pm blkData@ will write the contents of @blkData@
--   to the file pointed to by handle at the correct position in the file. If the
--   block is of a wrong length, the call will fail.
writeBlock :: Handles -> PieceNum -> Block -> PieceMap -> B.ByteString -> IO ()
writeBlock handles n blk pm blkData =
    {-# SCC "writeBlock" #-}
    do when lenFail $ fail "Writing block of wrong length"
       pInfo <- pInfoLookup n pm
       foldM_ (\blkD (pto, oto, szfrom) ->
                   do let size' = szfrom
                      (pfrom, ofrom, sz) <- return . BI.toForeignPtr $ B.take size' blkD
                      BI.memcpy (plusPtr (unsafeForeignPtrToPtr pto) oto)
                                (plusPtr (unsafeForeignPtrToPtr pfrom) ofrom) (fromIntegral sz)
                      return $ B.drop size' blkD
              ) blkData (projectHandles handles (fromIntegral . position $ pInfo) (fromIntegral $ B.length blkData))
       return ()
  where
    position :: PieceInfo -> Integer
    position pinfo = (offset pinfo) + fromIntegral (blockOffset blk)
    lenFail = B.length blkData /= blockSize blk

-- | The @checkPiece inf hs@ checks the file system for correctness of a given piece, namely if
--   the piece described by @inf@ is correct inside the file pointed to by @h@.
checkPiece :: PieceInfo -> Handles -> IO Bool
checkPiece inf handles = {-# SCC "checkPiece" #-} do
  bs <- return . L.fromChunks $ map (\(p, off, sz) -> BI.fromForeignPtr p off sz)
                     $ projectHandles handles (fromIntegral . offset $ inf) (fromInteger $ len inf)
  dgs <- liftIO $ D.digest bs
  return (dgs == digest inf)

-- | Create a MissingMap from a file handle and a piecemap. The system will read each part of
--   the file and then check it against the digest. It will create a map of what we are missing
--   in the file as a missing map. We could alternatively choose a list of pieces missing rather
--   then creating the data structure here. This is perhaps better in the long run.
checkFile :: Handles -> PieceMap -> IO PiecesDoneMap
checkFile handles pm = do l <- mapM checkP pieces
                          return $ M.fromList l
    where pieces = assocs pm
          checkP :: (PieceNum, PieceInfo) -> IO (PieceNum, Bool)
          checkP (pn, pInfo) = do b <- checkPiece pInfo handles
                                  return (pn, b)

-- | Extract the PieceMap from a bcoded structure
--   Needs some more defense in the long run.
mkPieceMap :: BCode -> Maybe PieceMap
mkPieceMap bc = fetchData
  where fetchData = do pLen <- infoPieceLength bc
                       pieceData <- infoPieces bc
                       tLen <- infoLength bc
                       let pis = extract pLen tLen 0 pieceData
                           l   = length pis
                           pm  = array (0, l-1) (zip [0..] pis)
                       when ( tLen /= (sum $ map len $ elems pm) )
                            (error "PieceMap construction size assertion failed")
                       return pm
        extract :: Integer -> Integer -> Integer -> [B.ByteString] -> [PieceInfo]
        extract _    0     _    []       = []
        extract plen tlen offst (p : ps) | tlen < plen = PieceInfo {
                                                          offset = offst,
                                                          len = tlen,
                                                          digest = p } : extract plen 0 (offst + plen) ps
                                  | otherwise = inf : extract plen (tlen - plen) (offst + plen) ps
                                       where inf = PieceInfo { offset = offst,
                                                               len = plen,
                                                               digest = p }
        extract _ _ _ _ = error "mkPieceMap: the impossible happened!"

-- | Predicate function. True if nothing is missing from the map.
canSeed :: PiecesDoneMap -> Bool
canSeed = M.fold (&&) True

-- | Process a BCoded torrent file. Create directories, open the files
--   in question, check it and return Handles plus a haveMap for the
--   file
openAndCheckFile :: BCode -> IO (Handles, PiecesDoneMap, PieceMap)
openAndCheckFile bc =
    do
       handles <- Handles `fmap`
                  forM files
                           (\(path, l) ->
                                do let dir = joinPath $ init path
                                   when (dir /= "") $
                                        createDirectoryIfMissing True dir
                                   let fpath = joinPath path
                                   q <- mapFile fpath (fromIntegral l)
                                   return q
                           )
       have <- checkFile handles pieceMap
       return (handles, have, pieceMap)
  where Just files = BCode.infoFiles bc
        Just pieceMap = mkPieceMap bc

mapFile :: FilePath -> Int -> IO MMapF
mapFile fpath l = do
    (p, off, sz) <- mmapFileForeignPtr fpath ReadWriteEx (Just (0, l))
    return $ MMapF p off sz
