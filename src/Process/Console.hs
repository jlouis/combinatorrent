-- | The Console process has two main purposes. It is a telnet-like
--   interface with the user and it is our first simple logging device
--   for what happens inside the system.
{-# LANGUAGE ScopedTypeVariables #-}
module Process.Console
    ( start
    )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader

import Prelude hiding (catch)


import Process
import qualified Process.Status as St
import Supervisor


data Cmd = Quit -- ^ Quit the program
         | Show -- ^ Show current state
         | Help -- ^ Print Help message
         | Unknown String -- ^ Unknown command
         deriving (Eq, Show)

type CmdChannel = TChan Cmd

data CF = CF { cmdCh :: CmdChannel
             , wrtCh :: TChan String }

instance Logging CF where
    logName _ = "Process.Console"

-- | Start the logging process and return a channel to it. Sending on this
--   Channel means writing stuff out on stdOut
start :: TMVar () -> St.StatusChannel -> SupervisorChan -> IO ThreadId
start waitC statusC supC = do
    cmdC <- readerP -- We shouldn't be doing this in the long run
    wrtC <- writerP
    spawnP (CF cmdC wrtC) () (catchP (forever lp) (defaultStopHandler supC))
  where
    lp = do
        c <- asks cmdCh
        o <- asks wrtCh
        m <- liftIO . atomically $ readTChan c
        case m of
            Quit -> liftIO . atomically $ putTMVar waitC ()
            Help -> liftIO . atomically $ writeTChan o helpMessage
            (Unknown n) -> liftIO . atomically $ writeTChan o $ "Uknown command: " ++ n
            Show -> do
                v <- liftIO newEmptyTMVarIO
                liftIO . atomically $ writeTChan statusC (St.RequestAllTorrents v)
                sts <- liftIO . atomically $ takeTMVar v
                liftIO . atomically $ writeTChan o (show sts)

helpMessage :: String
helpMessage = concat
    [ "Command Help:\n"
    , "\n"
    , "  help    - Show this help\n"
    , "  quit    - Quit the program\n"
    , "  show    - Show the current downloading status\n"
    ]

writerP :: IO (TChan String)
writerP = do wrt <- newTChanIO
             _ <- forkIO $ lp wrt
             return wrt
  where lp wCh = forever (do m <- atomically $ readTChan wCh
                             putStrLn m)

readerP :: IO CmdChannel
readerP = do cmd <- newTChanIO
             _ <- forkIO $ lp cmd
             return cmd
  where lp cmd = forever $
           do l <- getLine
              atomically $ writeTChan cmd
                (case l of
                   "help" -> Help
                   "quit" -> Quit
                   "show" -> Show
                   c      -> Unknown c)

