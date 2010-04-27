module Process.Peer.Receiver
    ( start )
where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString as B
import Prelude hiding (catch, log)

import qualified Data.Attoparsec as A

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString

import Process
import Supervisor
import Protocol.Wire


data CF = CF { rpMsgCh :: TChan (Message, Integer) }

instance Logging CF where
    logName _ = "Process.Peer.Receiver"

start :: Socket -> TChan (Message, Integer)
          -> SupervisorChannel -> IO ThreadId
start s ch supC = do
   spawnP (CF ch) s
        ({-# SCC "Receiver" #-} catchP readSend
               (defaultStopHandler supC))

readSend :: Process CF Socket ()
readSend = do
    s <- get
    c <- asks rpMsgCh
    bs <- liftIO $ recv s 2048
    when (B.length bs == 0) stopP
    loop s c (A.parse getMsg bs)
  where loop s c (A.Done r msg) = do
            liftIO . atomically $ writeTChan c (msg, fromIntegral $ msgSize msg)
            loop s c (A.parse getMsg r)
        loop s c (prt@(A.Partial _)) = do
            bs <- liftIO $ recv s 4096
            when (B.length bs == 0) stopP
            loop s c (A.feed prt bs)
        loop _ _ (A.Fail _ ctx err) =
                    do warningP $ "Incorrect parse in receiver, context: "
                                        ++ show ctx ++ ", " ++ show err
                       stopP

