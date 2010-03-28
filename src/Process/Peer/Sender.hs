module Process.Peer.Sender
  ( start )
where

import Control.Concurrent
import Control.Concurrent.CML.Strict

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString as B

import System.IO
import System.Timeout

import Process
import Protocol.Wire
import Supervisor

data CF = CF { chan :: Channel B.ByteString }

instance Logging CF where
    logName _ = "Process.Peer.Sender"

-- | The raw sender process, it does nothing but send out what it syncs on.
start :: Handle -> Channel B.ByteString -> SupervisorChan -> IO ThreadId
start h ch supC = spawnP (CF ch) h (catchP (foreverP pgm)
                                          (do t <- liftIO $ myThreadId
                                              syncP =<< (sendP supC $ IAmDying t)
                                              liftIO $ hClose h))

pgm :: Process CF Handle ()
pgm = {-# SCC "Peer.Sender" #-} do
        ch <- asks chan
        m <- liftIO $ timeout defaultTimeout (receiver ch)
        h <- get
        case m of
            Nothing -> putMsg (encodePacket KeepAlive)
            Just m  -> putMsg m
        liftIO $ hFlush h
  where
    putMsg m = do
        h <- get
        liftIO $ B.hPut h m

defaultTimeout :: Int
defaultTimeout = 120 * 1000000

receiver :: Channel a -> IO a
receiver ch = sync $ receive ch (const True)
