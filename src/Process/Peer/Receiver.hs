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

import Channels
import Process
import Supervisor
import Protocol.Wire


data CF = CF { rpMsgCh :: TChan MsgTy }

instance Logging CF where
    logName _ = "Process.Peer.Receiver"

start :: Socket -> TChan MsgTy -> SupervisorChannel -> IO ThreadId
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
    loop 2048 s c (A.parse getMsg bs)
  where loop l s c (A.Done r msg) = do
            liftIO . atomically $ writeTChan c (FromPeer (msg, fromIntegral $ msgSize msg))
            loop l s c (A.parse getMsg r)
        loop l s c (prt@(A.Partial _)) = do
            bs <- liftIO $ recv s l
            let k = B.length bs
            when (l == 0) stopP
            loop (nextSize l k) s c (A.feed prt bs)
        loop _ _ _ (A.Fail _ ctx err) =
                    do warningP $ "Incorrect parse in receiver, context: "
                                        ++ show ctx ++ ", " ++ show err
                       stopP
        nextSize l k | l == k = l*2
                     | l > (3*k) = if l > 2048 then l `div` 2 else 2048
                     | otherwise = l
