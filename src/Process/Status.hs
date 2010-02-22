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

-- | The status code runs a Status Process. This process keeps track
--   of a number of interval valies for a given torrent file and it
--   periodically updates the tracker process with the relevant
--   information about data uploaded, downloaded and how much is
--   left. The tracker is then responsible for using this data
--   correctly to tell the tracker what to do
module Process.Status (
    -- * Types
      StatusMsg(..)
    , TrackerMsg(..)
    -- * Channels
    , StatusChan
    -- * State
    , ST(uploaded, downloaded, state, left)
    -- * Interface
    , start
    )
where

import Control.Concurrent
import Control.Concurrent.CML

import Control.Monad.State

import Prelude hiding (log)
import Logging
import Process
import Supervisor
import Torrent

data StatusMsg = TrackerStat { trackIncomplete :: Maybe Integer
                             , trackComplete   :: Maybe Integer }
               | CompletedPiece Integer
               | PeerStat { peerUploaded :: Integer
                          , peerDownloaded :: Integer }
               | TorrentCompleted

type StatusChan = Channel StatusMsg

-- | TrackerChannel is the channel of the tracker
data TrackerMsg = Stop | TrackerTick Integer | Start | Complete

data CF  = CF { logCh :: LogChannel
              , statusCh :: Channel StatusMsg
              , trackerCh1 :: Channel TrackerMsg
              , trackerCh :: Channel ST }

instance Logging CF where
    getLogger cf = ("StatusP", logCh cf)

data ST = ST { uploaded :: Integer,
               downloaded :: Integer,
               left :: Integer,
               incomplete :: Maybe Integer,
               complete :: Maybe Integer,
               state :: TorrentState }

-- | Start a new Status process with an initial torrent state and a
--   channel on which to transmit status updates to the tracker.
start :: LogChannel -> Integer -> TorrentState -> Channel ST
      -> Channel StatusMsg -> Channel TrackerMsg -> SupervisorChan -> IO ThreadId
start logC l tState trackerC statusC trackerC1 supC = do
    spawnP (CF logC statusC trackerC1 trackerC) (ST 0 0 l Nothing Nothing tState)
        (catchP (foreverP pgm) (defaultStopHandler supC))
  where
    pgm = do ev <- chooseP [sendEvent, recvEvent]
             syncP ev
    sendEvent = get >>= sendPC trackerCh
    recvEvent = do evt <- recvPC statusCh
                   wrapP evt (\m ->
                    case m of
                        TrackerStat ic c ->
                           modify (\s -> s { incomplete = ic, complete = c })
                        CompletedPiece bytes -> do
                            logDebug "StatusProcess updated left"
                            modify (\s -> s { left = (left s) - bytes })
                        PeerStat up down -> do
                           modify (\s -> s { uploaded = (uploaded s) + up,
                                             downloaded = (downloaded s) + down })
                           u <- gets uploaded
                           d <- gets downloaded
                           logDebug $ "StatusProcess up/down count: " ++ show u ++ ", " ++ show d
                        TorrentCompleted -> do
                           logDebug "TorrentCompletion at StatusP"
                           l <- gets left
                           when (l /= 0) (fail "Warning: Left is not 0 upon Torrent Completion")
                           syncP =<< sendPC trackerCh1 Complete
                           modify (\s -> s { state = Seeding }))
                   
