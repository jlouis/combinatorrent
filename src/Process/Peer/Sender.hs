module Process.Peer.Sender
  ( start )
where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad.Reader

import qualified Data.ByteString.Lazy as L

import System.IO

import Process
import Supervisor

data CF = CF { chan :: TMVar L.ByteString
             , hndl :: Handle }

instance Logging CF where
    logName _ = "Process.Peer.Sender"

-- | The raw sender process, it does nothing but send out what it syncs on.
start :: Handle -> TMVar L.ByteString -> SupervisorChannel -> IO ThreadId
start h ch supC = spawnP (CF ch h) () (catchP pgm
                                          (do t <- liftIO $ myThreadId
                                              liftIO . atomically $ writeTChan supC $ IAmDying t
                                              liftIO $ hClose h))

pgm :: Process CF () ()
pgm = do
   ch <- asks chan
   h <- asks hndl
   liftIO $ do
      r <- atomically $ takeTMVar ch
      L.hPut h r
      hFlush h
   pgm

