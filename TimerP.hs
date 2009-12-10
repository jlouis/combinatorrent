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

-- | The timer module is responsible for timing in the project. With
--   the timer system, we can register timers in the future and then
--   we can get a tick triggered at the point in time where we need to
--   act. This allows us to postpone events into the future at a
--   designated time.
--
--   The timer keeps a queue of ticks and when to send them out. It
--   keeps the amount of work it needs to do down to a minimum to
--   keeping a delta-time for each queue element and in earliest first
--   order. This is the same way most operating systems handles time
--   tracking and jlouis first heard about it as used in the BSD4.4
--   Unix implementation.
--
--   TODO: Get the timing code to work.
--         Another approach is to simply run each timer as a new process and
--         then effectively use the underlying queue of the GHC system/OS for
--         the problem.
module TimerP (Tick(..),
               register)

where

import Control.Concurrent
import Control.Concurrent.CML

-- | A Tick is a single timer tick. It contains a version concept,
--   wherein an Integer defines what version we are currently waiting
--   for. The versioning allows silent cancel of future timer events
--   since a process can just ignore old ticks.
data Tick = Tick Int

-- | Registers a timer tick on a channel in a number of seconds with
--   an annotated version.
register :: Int -> Int -> Channel Tick -> IO ()
register secs version tickChan = do spawn timerProcess
                                    return ()
  where timerProcess = do threadDelay $ secs * 1000000
                          sync $ transmit tickChan (Tick version)

