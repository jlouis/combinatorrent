module Process.Peer.SenderQ
  ( SenderQMsg(..)
  , start
  )
where

import Control.Concurrent
import Control.Concurrent.STM

import Control.DeepSeq

import Control.Monad.Reader
import Control.Monad.State

import Prelude hiding (catch, log)

import qualified Data.ByteString as B

import Channels
import Process
import DeepSeqInstances()
import qualified Data.Queue as Q
import Supervisor
import Torrent
import Protocol.Wire

-- | Messages we can send to the Send Queue
data SenderQMsg = SenderQCancel PieceNum Block -- ^ Peer requested that we cancel a piece
                | SenderQM Message           -- ^ We want to send the Message to the peer
                | SenderOChoke                 -- ^ We want to choke the peer
                | SenderQRequestPrune PieceNum Block -- ^ Prune SendQueue of this (pn, blk) pair

instance NFData SenderQMsg where
  rnf a = a `seq` ()

data CF = CF { sqIn :: TChan SenderQMsg
             , sqOut :: TMVar B.ByteString
             , bandwidthCh :: BandwidthChannel
             }

data ST = ST { outQueue :: Q.Queue Message
             , bytesTransferred :: Integer
             }

instance Logging CF where
    logName _ = "Process.Peer.SendQueue"

-- | sendQueue Process, simple version.
--   TODO: Split into fast and slow.
start :: TChan SenderQMsg -> TMVar B.ByteString -> BandwidthChannel
           -> SupervisorChan
           -> IO ThreadId
start inC outC bandwC supC = spawnP (CF inC outC bandwC) (ST Q.empty 0)
        (catchP (foreverP pgm)
                (defaultStopHandler supC))

pgm :: Process CF ST ()
pgm = {-# SCC "Peer.SendQueue" #-} do
    q <- gets outQueue
    l <- gets bytesTransferred
    -- Gather together events which may trigger
    when (l > 0) rateUpdateEvent
    ic <- asks sqIn
    ov <- asks sqOut
    r <- case Q.first q of
        Nothing -> liftIO $ atomically (readTChan ic >>= return . Right)
        Just p -> do let bs = encodePacket p
                         sz = fromIntegral $ B.length bs
                     liftIO . atomically $
                         (putTMVar ov bs >> return (Left sz)) `orElse`
                         (readTChan ic >>= return . Right)
    case r of
        Left sz ->
            modify (\s -> s { bytesTransferred = bytesTransferred s + sz
                            , outQueue = Q.remove (outQueue s)})
        Right m ->
            case m of
                SenderQM msg -> modifyQ (Q.push msg)
                SenderQCancel n blk -> modifyQ (Q.filter (filterPiece n (blockOffset blk)))
                SenderOChoke -> do modifyQ (Q.filter filterAllPiece)
                                   modifyQ (Q.push Choke)
                SenderQRequestPrune n blk ->
                     modifyQ (Q.filter (filterRequest n blk))

rateUpdateEvent :: Process CF ST ()
rateUpdateEvent = {-# SCC "Peer.SendQ.rateUpd" #-} do
    l <- gets bytesTransferred
    bwc <- asks bandwidthCh
    liftIO . atomically $ writeTChan bwc l
    modify (\s -> s { bytesTransferred = 0 })

filterAllPiece :: Message -> Bool
filterAllPiece (Piece _ _ _) = True
filterAllPiece _             = False

filterPiece :: PieceNum -> Int -> Message -> Bool
filterPiece n off (Piece n1 off1 _) | n == n1 && off == off1 = False
                                    | otherwise               = True
filterPiece _ _   _                                           = True

filterRequest :: PieceNum -> Block -> Message -> Bool
filterRequest n blk (Request n1 blk1) | n == n1 && blk == blk1 = False
                                      | otherwise              = True
filterRequest _ _   _                                          = True

modifyQ :: (Q.Queue Message -> Q.Queue Message) -> Process CF ST ()
modifyQ f = modify (\s -> s { outQueue = f (outQueue s) })
