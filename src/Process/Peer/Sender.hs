module Process.Peer.Sender
  ( start )
where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad.Reader

import qualified Data.ByteString.Lazy as L

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)

import Process
import Supervisor

data CF = CF { chan :: TMVar L.ByteString
             , sock :: Socket }

instance Logging CF where
    logName _ = "Process.Peer.Sender"

-- | The raw sender process, it does nothing but send out what it syncs on.
start :: Socket -> TMVar L.ByteString -> SupervisorChannel -> IO ThreadId
start s ch supC = spawnP (CF ch s) () ({-# SCC "Sender" #-}
                                          (cleanupP pgm
                                            (defaultStopHandler supC)
                                            (liftIO $ close s)))
pgm :: Process CF () ()
pgm = do
   ch <- asks chan
   s <- asks sock
   _ <- liftIO $ do
      r <- atomically $ takeTMVar ch
      sendAll s r
   pgm

