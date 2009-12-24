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

-- | File system process. Acts as a maintainer for the filesystem in
--   question and can only do single-file torrents. It should be
--   fairly easy to add Multi-file torrents by altering this file and
--   the FS module.
module FSP
where

import Control.Concurrent.CML
import System.IO

import qualified Data.ByteString.Lazy as B

import ConsoleP
import Torrent
import qualified FS

data FSPMsg = ReadPiece PieceNum
            | ReadBlock PieceNum Block

type FSPChannel = Channel (FSPMsg, Channel B.ByteString)

data State = State {
      writeC :: Channel (PieceNum, Block, B.ByteString),
      rpcC :: FSPChannel,
      fileHandle :: Handle,
      pieceMap :: FS.PieceMap}


-- INTERFACE
----------------------------------------------------------------------

start :: Handle -> LogChannel -> FS.PieceMap -> IO (Channel (PieceNum, Block, B.ByteString),
                                                    Channel (FSPMsg, Channel B.ByteString))
start handle logC pm =
    do wc  <- channel
       rpcc <- channel
       spawn $ lp $ State wc rpcc handle pm
       return (wc, rpcc)
  where lp s = do s' <- sync $ choose [writeEvent s, readEvent s]
                  lp s'
        writeEvent s = wrap (receive (writeC s) (const True))
                         (\(pn, blk, bs) ->
                           do FS.writeBlock (fileHandle s) pn blk (pieceMap s) bs
                              return s)
        readEvent s  = wrap (receive (rpcC s) (const True))
                         (\(msg, c) ->
                              do bs <- case msg of
                                         ReadPiece pn -> do
                                                  logMsg logC $ "Reading piece #" ++ show pn
                                                  FS.readPiece pn (fileHandle s) (pieceMap s)
                                         ReadBlock pn blk -> do
                                                  logMsg logC $ "Reading block #" ++ show pn
                                                                  ++ "(" ++ show (blockOffset blk) ++ ", " ++ show (blockSize blk) ++ ")"
                                                  FS.readBlock pn blk (fileHandle s) (pieceMap s)
                                 sync $ transmit c bs
                                 return s)

readBlock :: FSPChannel -> Channel B.ByteString -> PieceNum -> Block -> IO ()
readBlock fspc c pn blk = sync $ transmit fspc (ReadBlock pn blk, c)

-- | Store a block in the file system.
storeBlock :: FSPChannel -> PieceNum -> Block -> B.ByteString -> IO ()
storeBlock = undefined

checkPiece :: FSPChannel -> PieceNum -> IO Bool
checkPiece = undefined

----------------------------------------------------------------------
