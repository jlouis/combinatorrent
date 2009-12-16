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
           readPiece,
           readBlock,
           writePiece,
           mkPieceMap,
           checkFile)
where


import qualified Data.ByteString.Lazy as B
import Data.Digest.Pure.SHA
import qualified Data.Map as M
import Data.Maybe
import System.IO

import BCode
import Torrent
import WireProtocol

type PieceMap = M.Map PieceNum PieceInfo


pInfoLookup :: PieceNum -> PieceMap -> IO PieceInfo
pInfoLookup pn mp = case M.lookup pn mp of
                      Nothing -> fail "FS: Error lookup in PieceMap"
                      Just i -> return i

readPiece :: PieceNum -> Handle -> PieceMap -> IO B.ByteString
readPiece pn handle mp =
    do pInfo <- pInfoLookup pn mp
       hSeek handle AbsoluteSeek (offset pInfo)
       bs <- B.hGet handle (fromInteger . len $ pInfo)
       if B.length bs == (fromInteger . len $ pInfo)
          then return bs
          else fail "FS: Wrong number of bytes read"

readBlock :: PieceNum -> PieceOffset -> PieceLength -> Handle -> PieceMap -> IO B.ByteString
readBlock pn os sz handle mp =
    do pInfo <- pInfoLookup pn mp
       hSeek handle AbsoluteSeek (offset pInfo + fromIntegral os)
       B.hGet handle (fromInteger sz)

writePiece :: PieceNum -> Handle -> PieceMap -> B.ByteString -> IO (Either String ())
writePiece pn handle mp bs =
    do pInfo <- pInfoLookup pn mp
       if (showDigest . sha1) bs /= digest pInfo
         then return $ Left "PieceCheck Error"
         else do hSeek handle AbsoluteSeek (offset pInfo)
                 B.hPut handle bs -- Will always get the right size due to SHA the digest
                 return $ Right ()

-- | Create a MissingMap from a file handle and a piecemap. The system will read each part of
--   the file and then check it against the digest. It will create a map of what we are missing
--   in the file as a missing map. We could alternatively choose a list of pieces missing rather
--   then creating the data structure here. This is perhaps better in the long run.
checkFile :: Handle -> PieceMap -> IO MissingMap
checkFile handle pm = do l <- mapM checkPiece pieces
                         return $ M.fromList l
    where pieces = M.toAscList pm
          checkPiece :: (Integer, PieceInfo) -> IO (Integer, Bool)
          checkPiece (pn, pInfo) =
              do hSeek handle AbsoluteSeek (offset pInfo) -- We assume this seek is good.
                 bs <- B.hGet handle (fromInteger . len $ pInfo)
                 return (pn, (showDigest . sha1) bs == digest pInfo)

-- | Extract the PieceMap from a bcoded structure
--   Needs some more defense in the long run.
mkPieceMap :: BCode -> Maybe PieceMap
mkPieceMap bc = fetchData
  where fetchData = do pLen <- infoPieceLength bc
                       pieceData <- infoPieces bc
                       tLen <- infoLength bc
                       return $ M.fromList $ zip [1..] $ extract pLen tLen 0 pieceData
        extract :: Integer -> Integer -> Integer -> [String] -> [PieceInfo]
        extract _  0  _  [] = []
        extract pl tl os (p : ps) | tl < pl = PieceInfo { offset = os,
                                                          len = tl,
                                                          digest = p } : extract pl 0 (os + pl) ps
                                  | otherwise = inf : extract pl (tl - pl) (os + pl) ps
                                       where inf = PieceInfo { offset = os,
                                                               len = pl,
                                                               digest = p }
        extract _ _ _ [] = undefined -- Can never be hit (famous last words)








