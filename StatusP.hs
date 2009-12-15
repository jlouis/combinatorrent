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
module StatusP (TorrentState(..),
                State(uploaded, downloaded, state, left),
                start)
where

import Control.Concurrent.CML

import ConsoleP (LogChannel, logMsg)

data TorrentState = Seeding | Leeching

data State = MkState { uploaded :: Integer,
                       downloaded :: Integer,
                       left :: Integer,
                       incomplete :: Integer,
                       complete :: Integer,
                       state :: TorrentState }

-- | Start a new Status process with an initial torrent state and a
--   channel on which to transmit status updates to the tracker.
start :: LogChannel -> Integer -> TorrentState -> Channel State
      -> Channel (Integer, Integer) -> IO ()
start logCh l tstate trackerChanOut statusChan = lp $ MkState 0 0 l 0 0 tstate
  where lp s = do s' <- sync $ choose [sendEvent s, recvEvent s]
                  lp s'
        sendEvent s = wrap (transmit trackerChanOut s)
                        (\_ -> do logMsg logCh "Sending event to Tracker"
                                  return s)
        recvEvent s = wrap (receive statusChan (const True))
                        (\(ic, c) -> do logMsg logCh "Receiving event from Tracker"
                                        return s { incomplete = ic, complete = c})
