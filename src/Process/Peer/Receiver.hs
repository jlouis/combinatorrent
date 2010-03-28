module Process.Peer.Receiver
    ( start )
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.CML.Strict
import Control.Concurrent.STM

import Control.Monad.State
import Control.Monad.Reader

import Prelude hiding (catch, log)

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Serialize.Get as G

import qualified Data.Map as M
import qualified Data.PieceSet as PS
import Data.Maybe

import Data.Set as S hiding (map)
import Data.Time.Clock
import Data.Word

import System.IO

import PeerTypes
import Process
import RateCalc as RC
import Supervisor
import Process.Timer as Timer
import Torrent
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
