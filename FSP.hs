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
import WireProtocol

data FSPMsg = ReadPiece PieceNum
            | ReadBlock PieceNum PieceOffset PieceLength

type FSPChannel = Channel (FSPMsg, Channel B.ByteString)

data State = State {
      writeC :: Channel (PieceNum, B.ByteString),
      rpcC :: FSPChannel,
      fileHandle :: Handle,
      pieceMap :: FS.PieceMap}



start :: Handle -> LogChannel -> FS.PieceMap -> IO (Channel (PieceNum, B.ByteString),
                                                    Channel (FSPMsg, Channel B.ByteString))
start handle logC pm =
    do wc  <- channel
       rpcc <- channel
       spawn $ lp $ State wc rpcc handle pm
       return (wc, rpcc)
  where lp s = do s' <- sync $ choose [writeEvent s, readEvent s]
                  lp s'
        writeEvent s = wrap (receive (writeC s) (const True))
                         (\(pn, bs) ->
                           do FS.writePiece pn (fileHandle s) (pieceMap s) bs
                              return s)
        readEvent s  = wrap (receive (rpcC s) (const True))
                         (\(msg, c) ->
                              do bs <- case msg of
                                         ReadPiece pn -> do
                                                  logMsg logC $ "Reading piece #" ++ show pn
                                                  FS.readPiece pn (fileHandle s) (pieceMap s)
                                         ReadBlock pn os sz -> do
                                                  logMsg logC $ "Reading block #" ++ show pn
                                                                  ++ "(" ++ show os ++ ", " ++ show sz ++ ")"
                                                  FS.readBlock pn os sz (fileHandle s) (pieceMap s)
                                 sync $ transmit c bs
                                 return s)

readBlock :: FSPChannel -> Channel B.ByteString -> PieceNum -> PieceOffset -> PieceLength -> IO ()
readBlock fspc c pn os sz = sync $ transmit fspc (ReadBlock pn os sz, c)
