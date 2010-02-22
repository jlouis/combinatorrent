-- | The Console process has two main purposes. It is a telnet-like
--   interface with the user and it is our first simple logging device
--   for what happens inside the system.
{-# LANGUAGE ScopedTypeVariables #-}
module Process.Console
    ( start
    )
where

import Control.Concurrent
import Control.Concurrent.CML
import Control.Monad.Reader

import Prelude hiding (catch)
import Process

import Logging
import Supervisor

data Cmd = Quit -- Quit the program
         deriving (Eq, Show)

type CmdChannel = Channel Cmd

data CF = CF { cmdCh :: CmdChannel
             , logCh :: LogChannel }

instance Logging CF where
    getLogger cf = ("ConsoleP", logCh cf)

-- | Start the logging process and return a channel to it. Sending on this
--   Channel means writing stuff out on stdOut
start :: LogChannel -> Channel () -> SupervisorChan -> IO ThreadId
start logC waitC supC = do
    cmdC <- readerP logC -- We shouldn't be doing this in the long run
    spawnP (CF cmdC logC) () (catchP (forever lp) (defaultStopHandler supC))
  where
    lp = syncP =<< quitEvent
    quitEvent = do
        ch <- asks cmdCh
        ev <- recvP ch (==Quit)
        wrapP ev 
            (\_ -> syncP =<< sendP waitC ())
        

readerP :: LogChannel -> IO CmdChannel
readerP logCh = do cmdCh <- channel
                   spawn $ lp cmdCh
                   return cmdCh
  where lp cmdCh = do c <- getLine
                      case c of
                        "quit" -> sync $ transmit cmdCh Quit
                        cmd    -> do logMsg' logCh "Console" Info $ "Unrecognized command: " ++ show cmd
                                     lp cmdCh

