-- Haskell Torrent
-- Copyright (c) 2009, Jesper Louis Andersen,
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--  * Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

import Control.Monad
import Control.Monad.State

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Maybe
import System.IO
import System.Directory (createDirectoryIfMissing)
import Data.List (intercalate)

import BCode
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
newtype Handles = Handles [(Handle, Integer)]  -- ^[(fileHandle, fileLength)]

projectHandles :: Handles
               -> Integer    -- ^Torrent offset
               -> Integer    -- ^Torrent size
               -> [(Handle   -- ^File handle
                   ,Integer  -- ^File chunk offset
                   ,Integer  -- ^File chunk size
                   )]
{-
projectHandles handles offset size = let r = projectHandles' handles offset size
                                     in trace ("projectHandles " ++
                                               show handles ++ " " ++
                                               show offset ++ " " ++
                                               show size ++ " = " ++
                                               show r
                                              ) $
                                        r
-}
projectHandles (Handles handles@((h1, length1):handles')) offset size
    | size <= 0 =
        fail "FS: Should have already stopped projection"
    | null handles =
        fail "FS: Attempt to read beyond torrent length"
    | offset >= length1 =
        projectHandles (Handles handles') (offset - length1) size
    | otherwise =
        let size1 = length1 - offset  -- ^How much of h1 to take?
        in if size1 >= size
           then [(h1, offset, size)]
           else (h1, offset, size1) :
                projectHandles (Handles handles') 0 (size - size1)

pInfoLookup :: PieceNum -> PieceMap -> IO PieceInfo
pInfoLookup pn mp = case M.lookup pn mp of
                      Nothing -> fail "FS: Error lookup in PieceMap"
                      Just i -> return i

-- | FIXME: minor code duplication with @readBlock@
readPiece :: PieceNum -> Handles -> PieceMap -> IO L.ByteString
readPiece pn handles mp =
    do pInfo <- pInfoLookup pn mp
       bs <- L.concat `fmap`
             forM (projectHandles handles (offset pInfo) (len pInfo))
                      (\(h, offset, size) ->
                           do hSeek h AbsoluteSeek offset
                              L.hGet h (fromInteger size)
                      )
       if L.length bs == (fromInteger . len $ pInfo)
          then return bs
          else fail "FS: Wrong number of bytes read"

-- | FIXME: concatenating strict ByteStrings may turn out
--   expensive. Returning lazy ones may be more appropriate.
readBlock :: PieceNum -> Block -> Handles -> PieceMap -> IO B.ByteString
readBlock pn blk handles mp =
    do pInfo <- pInfoLookup pn mp
       B.concat `fmap`
        forM (projectHandles handles (offset pInfo + (fromIntegral $ blockOffset blk))
                                 (fromIntegral $ blockSize blk))
                 (\(h, offset, size) ->
                      do hSeek h AbsoluteSeek offset
                         B.hGet h $ fromInteger size
                 )

-- | The call @writeBlock h n blk pm blkData@ will write the contents of @blkData@
--   to the file pointed to by handle at the correct position in the file. If the
--   block is of a wrong length, the call will fail.
writeBlock :: Handles -> PieceNum -> Block -> PieceMap -> B.ByteString -> IO ()
writeBlock handles n blk pm blkData =
    do when lenFail $ fail "Writing block of wrong length"
       pInfo <- pInfoLookup n pm
       foldM_ (\blkData (h, offset, size) ->
                   do let size' = fromInteger size
                      hSeek h AbsoluteSeek offset
                      B.hPut h $ B.take size' blkData
                      hFlush h
                      return $ B.drop size' blkData
              ) blkData (projectHandles handles (position pInfo) (fromIntegral $ B.length blkData))
       return ()
  where
    position :: PieceInfo -> Integer
    position pinfo = (offset pinfo) + fromIntegral (blockOffset blk)
    lenFail = B.length blkData /= blockSize blk

-- | The @checkPiece h inf@ checks the file system for correctness of a given piece, namely if
--   the piece described by @inf@ is correct inside the file pointed to by @h@.
checkPiece :: PieceInfo -> Handles -> IO Bool
checkPiece inf handles = do
  bs <- L.concat `fmap`
        forM (projectHandles handles (offset inf) (fromInteger $ len inf))
                 (\(h, offset, size) ->
                      do hSeek h AbsoluteSeek offset
                         L.hGet h (fromInteger size)
                 )
  dgs <- liftIO $ D.digest bs
  return (dgs == digest inf)

-- | Create a MissingMap from a file handle and a piecemap. The system will read each part of
--   the file and then check it against the digest. It will create a map of what we are missing
--   in the file as a missing map. We could alternatively choose a list of pieces missing rather
--   then creating the data structure here. This is perhaps better in the long run.
checkFile :: Handles -> PieceMap -> IO PiecesDoneMap
checkFile handles pm = do l <- mapM checkP pieces
                          return $ M.fromList l
    where pieces = M.toAscList pm
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
		       let pm = M.fromList . zip [0..] . extract pLen tLen 0 $ pieceData
		       when ( tLen /= (sum $ map len $ M.elems pm) )
			    (error "PieceMap construction size assertion failed")
		       return pm
        extract :: Integer -> Integer -> Integer -> [B.ByteString] -> [PieceInfo]
        extract _    0     _    []       = []
        extract plen tlen offst (p : ps) | tlen < plen = PieceInfo { offset = offst,
                                                          len = tlen,
                                                          digest = B.unpack p } : extract plen 0 (offst + plen) ps
                                  | otherwise = inf : extract plen (tlen - plen) (offst + plen) ps
                                       where inf = PieceInfo { offset = offst,
                                                               len = plen,
                                                               digest = B.unpack p }
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
                           (\(path, length) ->
                                do let dir = joinPath $ init path
                                   when (dir /= "") $
                                        createDirectoryIfMissing True dir
                                   let fpath = joinPath path
                                   h <- openBinaryFile fpath ReadWriteMode
                                   return (h, length)
                           )
       have <- checkFile handles pieceMap
       return (handles, have, pieceMap)
  where Just files = BCode.infoFiles bc
        Just pieceMap = mkPieceMap bc
        joinPath = intercalate "/"





