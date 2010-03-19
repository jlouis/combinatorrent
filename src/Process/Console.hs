-- | The Console process has two main purposes. It is a telnet-like
--   interface with the user and it is our first simple logging device
--   for what happens inside the system.
{-# LANGUAGE ScopedTypeVariables #-}
module Process.Console
    ( start
    )
where

import Control.Concurrent
import Control.Concurrent.CML.Strict
import Control.DeepSeq
import Control.Monad.Reader

import Prelude hiding (catch)
import System.Log.Logger


import Process
import qualified Process.Status as St
import Supervisor


data Cmd = Quit -- ^ Quit the program
         | Show -- ^ Show current state
         | Help -- ^ Print Help message
         | Unknown String -- ^ Unknown command
         deriving (Eq, Show)

instance NFData Cmd where
  rnf a = a `seq` ()

type CmdChannel = Channel Cmd

data CF = CF { cmdCh :: CmdChannel
             , wrtCh :: Channel String }

instance Logging CF where
    logName _ = "Process.Console"

-- | Start the logging process and return a channel to it. Sending on this
--   Channel means writing stuff out on stdOut
start :: Channel () -> Channel St.StatusMsg -> SupervisorChan -> IO ThreadId
start waitC statusC supC = do
    cmdC <- readerP -- We shouldn't be doing this in the long run
    wrtC <- writerP
    spawnP (CF cmdC wrtC) () (catchP (forever lp) (defaultStopHandler supC))
  where
    lp = syncP =<< chooseP [quitEvent, helpEvent, unknownEvent, showEvent]
    quitEvent = do
        ch <- asks cmdCh
        ev <- recvP ch (==Quit)
        wrapP ev 
            (\_ -> syncP =<< sendP waitC ())
    helpEvent = do
        ch <- asks cmdCh
        wrtC <- asks wrtCh
        ev <- recvP ch (==Help)
        wrapP ev
            (\_ -> syncP =<< sendPC wrtCh helpMessage)
    unknownEvent = do
        ch <- asks cmdCh
        wrtC <- asks wrtCh
        ev <- recvP ch (\m -> case m of
                                Unknown _ -> True
                                _         -> False)
        wrapP ev
            (\(Unknown cmd) -> syncP =<< (sendPC wrtCh $ "Unknown command: " ++ cmd))
    showEvent = do
        ch <- asks cmdCh
        wrtC <- asks wrtCh
        ev <- recvP ch (==Show)
        wrapP ev
            (\_ -> do
                ch <- liftIO $ channel
                syncP =<< sendP statusC (St.RequestAllTorrents ch)
                sts <- syncP =<< recvP ch (const True)
                syncP =<< sendPC wrtCh (show sts))

helpMessage :: String
helpMessage = concat
    [ "Command Help:\n"
    , "\n"
    , "  help    - Show this help\n"
    , "  quit    - Quit the program\n"
    , "  show    - Show the current downloading status\n"
    ]

writerP :: IO (Channel String)
writerP = do wrtCh <- channel
             spawn $ lp wrtCh
             return wrtCh
  where lp wCh = forever (do m <- sync $ receive wCh (const True)
                             putStrLn m)

readerP :: IO CmdChannel
readerP = do cmdCh <- channel
             spawn $ lp cmdCh
             return cmdCh
  where lp cmdCh = forever $
           do c <- getLine
              case c of
                "help" -> sync $ transmit cmdCh Help
                "quit" -> sync $ transmit cmdCh Quit
                "show" -> sync $ transmit cmdCh Show
                cmd    -> do logM "Process.Console.readerP" INFO $
                                     "Unrecognized command: " ++ show cmd
                             sync $ transmit cmdCh (Unknown cmd)

