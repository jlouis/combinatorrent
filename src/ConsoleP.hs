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

-- | The Console process has two main purposes. It is a telnet-like
--   interface with the user and it is our first simple logging device
--   for what happens inside the system.
module ConsoleP (LogChannel,
                 start,
                 logMsg)
where

import Control.Concurrent.CML

data LogPriority = Low
                 | Default
                 | System
                 | High
                 | Fatal
                 deriving Show

data Message = Mes LogPriority String

type LogChannel = Channel Message

data Cmd = Quit -- Quit the program
         deriving (Eq, Show)

type CmdChannel = Channel Cmd

-- | Log a message to a channel
logMsg :: LogChannel -> String -> IO ()
logMsg c str = sync . transmit c $ Mes Default str

-- | Log a message to a channel with a priority
logMsg' :: LogChannel -> LogPriority -> String -> IO ()
logMsg' c pri str = sync . transmit c $ Mes pri str

-- | Start the logging process and return a channel to it. Sending on this
--   Channel means writing stuff out on stdOut
start :: Channel () -> IO (Channel Message)
start waitCh = do c <- channel
                  cmdCh <- readerP c
                  spawn (logger cmdCh c)
                  return c
  where logger cmdCh logCh = do sync $ choose [logEvent logCh,
                                               quitEvent cmdCh]
                                logger cmdCh logCh
        logEvent logCh = wrap (receive logCh (const True))
                           print
        quitEvent ch = wrap (receive ch (==Quit))
                     (\_ -> sync $ transmit waitCh ())





readerP :: LogChannel -> IO CmdChannel
readerP logCh = do cmdCh <- channel
                   spawn $ lp cmdCh
                   return cmdCh
  where lp cmdCh = do c <- getLine
                      case c of
                        "quit" -> sync $ transmit cmdCh Quit
                        cmd    -> do logMsg' logCh Low $ "Unrecognized command: " ++ show cmd
                                     lp cmdCh


instance Show Message where
    show (Mes pri str) = show pri ++ ":\t" ++ str