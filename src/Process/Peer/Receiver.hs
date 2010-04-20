module Process.Peer.Receiver
    ( start )
where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad.Reader
import Control.Monad.State

import Prelude hiding (catch, log)

import qualified Data.Attoparsec as A
import qualified Data.ByteString as B

import System.IO

import Process
import Supervisor
import Protocol.Wire


data CF = CF { rpMsgCh :: TChan (Message, Integer) }

instance Logging CF where
    logName _ = "Process.Peer.Receiver"

start :: Handle -> TChan (Message, Integer)
          -> SupervisorChannel -> IO ThreadId
start h ch supC = do
   hSetBuffering h NoBuffering
   spawnP (CF ch) h
        (catchP readSend
               (defaultStopHandler supC))

readSend :: Process CF Handle ()
readSend = do
    h <- get
    c <- asks rpMsgCh
    _ <- liftIO $ hWaitForInput h (-1)
    bs <- liftIO $ B.hGetNonBlocking h 8192
    loop h c (A.parse getMsg bs)
  where loop h c (A.Done r msg) = do
            liftIO . atomically $ writeTChan c (msg, fromIntegral $ msgSize msg)
            loop h c (A.parse getMsg r)
        loop h c (prt@(A.Partial _)) = do
            _ <- liftIO $ hWaitForInput h (-1)
            bs <- liftIO $ B.hGetNonBlocking h 8192
            loop h c (A.feed prt bs)
        loop _ _ (A.Fail _ _ _) =
                    do warningP "Incorrect parse in receiver, dying!"
                       stopP

