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

import qualified Data.ByteString as B
import qualified Data.Map as M

import ConsoleP
import Torrent
import qualified FS

data FSPMsg = CheckPiece PieceNum (Channel (Maybe Bool))
            | WriteBlock PieceNum Block B.ByteString
            | ReadBlock PieceNum Block (Channel B.ByteString)

type FSPChannel = Channel FSPMsg

data State = State {
      fspCh :: FSPChannel, -- ^ Channel on which to receive messages
      fileHandle :: Handle, -- ^ The file we are working on
      pieceMap :: FS.PieceMap -- ^ Map of where the pieces reside
    }


-- INTERFACE
----------------------------------------------------------------------

start :: Handle -> LogChannel -> FS.PieceMap -> IO FSPChannel
start handle logC pm =
    do fspC <- channel
       spawn $ lp $ State fspC handle pm
       return fspC
  where lp s = sync (msgEvent s) >>= lp
        msgEvent s = wrap (receive (fspCh s) (const True))
                       -- TODO: Coalesce common 'return s'
                       (\m -> case m of
                               CheckPiece n ch ->
                                   case M.lookup n (pieceMap s) of
                                     Nothing -> do sync $ transmit ch Nothing
                                                   return s
                                     Just pi -> do r <- FS.checkPiece (fileHandle s) pi
                                                   sync $ transmit ch (Just r)
                                                   return s
                               ReadBlock n blk ch -> do
                                   logMsg logC $ "Reading block #" ++ show n
                                     ++ "(" ++ show (blockOffset blk) ++ ", " ++ show (blockSize blk) ++ ")"
                                   -- TODO: Protection, either here or in the Peer code
                                   bs <- FS.readBlock n blk (fileHandle s) (pieceMap s)
                                   sync $ transmit ch bs
                                   return s
                               WriteBlock pn blk bs -> do
                                   -- TODO: Protection, either here or in the Peer code
                                   FS.writeBlock (fileHandle s) pn blk (pieceMap s) bs
                                   return s)

readBlock :: FSPChannel -> Channel B.ByteString -> PieceNum -> Block -> IO ()
readBlock fspc c pn blk = sync $ transmit fspc $ ReadBlock pn blk c

-- | Store a block in the file system.
storeBlock :: FSPChannel -> PieceNum -> Block -> B.ByteString -> IO ()
storeBlock fspC n blk bs = sync $ transmit fspC $ WriteBlock n blk bs

checkPiece :: FSPChannel -> PieceNum -> IO (Maybe Bool)
checkPiece fspC n = do
    ch <- channel
    sync . transmit fspC $ CheckPiece n ch
    sync $ receive ch (const True)


----------------------------------------------------------------------
