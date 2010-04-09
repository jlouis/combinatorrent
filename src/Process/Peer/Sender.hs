module Process.Peer.Sender
  ( start )
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString as B

import System.IO

import Process
import Protocol.Wire
import Supervisor

data CF = CF { chan :: TMVar B.ByteString }

instance Logging CF where
    logName _ = "Process.Peer.Sender"

-- | The raw sender process, it does nothing but send out what it syncs on.
start :: Handle -> TMVar B.ByteString -> SupervisorChannel -> IO ThreadId
start h ch supC = spawnP (CF ch) h (catchP (forever pgm)
                                          (do t <- liftIO $ myThreadId
                                              liftIO . atomically $ writeTChan supC $ IAmDying t
                                              liftIO $ hClose h))

pgm :: Process CF Handle ()
pgm = {-# SCC "Peer.Sender" #-} do
        ch <- asks chan
        tout <- liftIO $ registerDelay defaultTimeout
        r <- liftIO . atomically $ do
            t <- readTVar tout
            if t
                then return Nothing
                else Just <$> takeTMVar ch
        h <- get
        case r of
            Nothing -> putMsg (encodePacket KeepAlive)
            Just m  -> putMsg m
        liftIO $ hFlush h
  where
    putMsg m = do
        h <- get
        liftIO $ B.hPut h m

defaultTimeout :: Int
defaultTimeout = 120 * 1000000
