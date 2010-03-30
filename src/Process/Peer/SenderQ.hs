module Process.Peer.SenderQ
  ( SenderQMsg(..)
  , start
  )
where

import Control.Concurrent
import Control.Concurrent.CML.Strict

import Control.DeepSeq

import Control.Monad.State

import Prelude hiding (catch, log)

import qualified Data.ByteString as B

import PeerTypes
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

data CF = CF { sqInCh :: Channel SenderQMsg
             , sqOutCh :: Channel B.ByteString
             , bandwidthCh :: BandwidthChannel
             }

data ST = ST { outQueue :: Q.Queue Message
             , bytesTransferred :: Integer
             }

instance Logging CF where
    logName _ = "Process.Peer.SendQueue"

-- | sendQueue Process, simple version.
--   TODO: Split into fast and slow.
start :: Channel SenderQMsg -> Channel B.ByteString -> BandwidthChannel
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
    syncP =<< (chooseP $
        concat [if Q.isEmpty q then [] else [sendEvent],
                if l > 0 then [rateUpdateEvent] else [],
                [queueEvent]])

rateUpdateEvent :: Process CF ST (Event ((), ST))
rateUpdateEvent = {-# SCC "Peer.SendQ.rateUpd" #-} do
    l <- gets bytesTransferred
    ev <- sendPC bandwidthCh l
    wrapP ev (\() ->
        modify (\s -> s { bytesTransferred = 0 }))

queueEvent :: Process CF ST (Event ((), ST))
queueEvent = {-# SCC "Peer.SendQ.queueEvt" #-} do
    recvWrapPC sqInCh
            (\m -> case m of
                SenderQM msg -> modifyQ (Q.push msg)
                SenderQCancel n blk -> modifyQ (Q.filter (filterPiece n (blockOffset blk)))
                SenderOChoke -> do modifyQ (Q.filter filterAllPiece)
                                   modifyQ (Q.push Choke)
                SenderQRequestPrune n blk ->
                     modifyQ (Q.filter (filterRequest n blk)))

sendEvent :: Process CF ST (Event ((), ST))
sendEvent = {-# SCC "Peer.SendQ.sendEvt" #-} do
    Just (e, r) <- gets (Q.pop . outQueue)
    let bs = encodePacket e
    tEvt <- sendPC sqOutCh bs
    wrapP tEvt (\() -> modify (\s ->
            s { outQueue = r,
                bytesTransferred =
                bytesTransferred s + fromIntegral (B.length bs)}))

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
