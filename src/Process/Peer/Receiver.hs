module Process.Peer.Receiver
    ( start )
where

import Control.Concurrent
import Control.Concurrent.CML.Strict

import Control.Monad.State

import Prelude hiding (catch, log)

import qualified Data.ByteString as B
import qualified Data.Serialize.Get as G


import Data.Word

import System.IO

import Process
import Supervisor
import Protocol.Wire


data CF = CF { rpMsgCh :: Channel (Message, Integer) }

instance Logging CF where
    logName _ = "Process.Peer.Receiver"

start :: Handle -> Channel (Message, Integer)
          -> SupervisorChan -> IO ThreadId
start h ch supC = spawnP (CF ch) h
        (catchP (foreverP readSend)
               (defaultStopHandler supC))

readSend :: Process CF Handle ()
readSend = {-# SCC "Recv_readHeader" #-} do
    h <- get
    bs' <- liftIO $ B.hGet h 4
    l <- conv bs'
    if (l == 0)
        then return ()
        else do debugP $ "Reading off " ++ show l ++ " bytes"
                bs <- {-# SCC "Recv_hGet" #-} liftIO $ B.hGet h (fromIntegral l)
                case G.runGet decodeMsg bs of
                    Left _ -> do warningP "Incorrect parse in receiver, dying!"
                                 stopP
                    Right msg -> sendPC rpMsgCh (msg, fromIntegral l) >>= syncP

conv :: B.ByteString -> Process CF Handle Word32
conv bs = {-# SCC "Recv_conf" #-} do
    case G.runGet G.getWord32be bs of
      Left err -> do warningP $ "Incorrent parse in receiver, dying: " ++ show err
                     stopP
      Right i -> return i
