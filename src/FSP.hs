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
{-# LANGUAGE ScopedTypeVariables #-}
module FSP
    ( FSPChannel
    , FSPMsg(..)
    , start
    )
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.CML
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import System.IO


import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

import Logging
import Process
import Torrent
import qualified FS
import Supervisor

data FSPMsg = CheckPiece PieceNum (Channel (Maybe Bool))
            | WriteBlock PieceNum Block B.ByteString
            | ReadBlock PieceNum Block (Channel B.ByteString)

type FSPChannel = Channel FSPMsg

data CF = CF
      { fspCh :: FSPChannel -- ^ Channel on which to receive messages
      , logCh :: LogChannel -- ^ Channel for logging
      }

instance Logging CF where
  getLogger cf = ("FSP", logCh cf)

data ST = ST
      { fileHandle :: Handle -- ^ The file we are working on
      , pieceMap :: FS.PieceMap -- ^ Map of where the pieces reside
      }


-- INTERFACE
----------------------------------------------------------------------

start :: Handle -> LogChannel -> FS.PieceMap -> FSPChannel -> SupervisorChan-> IO ThreadId
start handle logC pm fspC supC =
    spawnP (CF fspC logC) (ST handle pm) (catchP (forever lp) (defaultStopHandler supC))
  where
    lp = msgEvent >>= syncP
    msgEvent = do
	ev <- recvPC fspCh
	wrapP ev (\msg ->
	    case msg of
		CheckPiece n ch -> do
		    pm <- gets pieceMap
		    case M.lookup n pm of
			Nothing -> sendP ch Nothing >>= syncP
			Just pi -> do r <- gets fileHandle >>= (liftIO . FS.checkPiece pi)
				      sendP ch (Just r) >>= syncP
		ReadBlock n blk ch -> do
		    logDebug $ "Reading block #" ++ show n
			    ++ "(" ++ show (blockOffset blk) ++ ", " ++ show (blockSize blk) ++ ")"
		    -- TODO: Protection, either here or in the Peer code
		    h  <- gets fileHandle
		    bs <- gets pieceMap >>= (liftIO . FS.readBlock n blk h)
		    sendP ch bs >>= syncP
		WriteBlock pn blk bs -> do
                    -- TODO: Protection, either here or in the Peer code
		    fh <- gets fileHandle
		    pm <- gets pieceMap
		    liftIO $ FS.writeBlock fh pn blk pm bs)

checkPiece :: FSPChannel -> PieceNum -> IO (Maybe Bool)
checkPiece fspC n = do
    ch <- channel
    sync . transmit fspC $ CheckPiece n ch
    sync $ receive ch (const True)

