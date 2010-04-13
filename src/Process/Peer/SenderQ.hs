module Process.Peer.SenderQ
  ( SenderQMsg(..)
  , start
  )
where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad.Reader
import Control.Monad.State

import Prelude hiding (catch, log)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Channels
import Process
import Process.FS hiding (start)
import qualified Data.Queue as Q
import Supervisor
import Torrent
import Protocol.Wire

-- | Messages we can send to the Send Queue
data SenderQMsg = SenderQCancel PieceNum Block -- ^ Peer requested that we cancel a piece
                | SenderQM Message           -- ^ We want to send the Message to the peer
                | SenderQPiece PieceNum Block -- ^ Request for a piece transmit
                | SenderOChoke                 -- ^ We want to choke the peer
                | SenderQRequestPrune PieceNum Block -- ^ Prune SendQueue of this (pn, blk) pair

data CF = CF { sqIn :: TChan SenderQMsg
             , sqOut :: TMVar L.ByteString
             , bandwidthCh :: BandwidthChannel
             , readBlockTV :: TMVar B.ByteString
             , fsCh        :: FSPChannel
             }

data ST = ST { outQueue         :: !(Q.Queue (Either Message (PieceNum, Block)))
             , bytesTransferred :: !Integer
             }

instance Logging CF where
    logName _ = "Process.Peer.SendQueue"

-- | sendQueue Process, simple version.
--   TODO: Split into fast and slow.
start :: TChan SenderQMsg -> TMVar L.ByteString -> BandwidthChannel
      -> FSPChannel -> SupervisorChannel -> IO ThreadId
start inC outC bandwC fspC supC = do
    rbtv <- liftIO newEmptyTMVarIO
    spawnP (CF inC outC bandwC rbtv fspC) (ST Q.empty 0)
        (catchP (forever pgm)
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
        Just r -> do p <- case r of
                            Left m -> return m
                            Right (pn, blk) -> do d <- readBlock pn blk
                                                  return $ Piece pn (blockOffset blk) d
                     let bs = encodePacket p
                         sz = fromIntegral $ L.length bs
                     liftIO . atomically $
                         (putTMVar ov bs >> return (Left sz)) `orElse`
                         (readTChan ic >>= return . Right)
    case r of
        Left sz ->
            modify (\s -> s { bytesTransferred = bytesTransferred s + sz
                            , outQueue = Q.remove (outQueue s)})
        Right m ->
            case m of
                SenderQM msg -> modifyQ (Q.push $ Left msg)
                SenderQPiece n blk -> modifyQ (Q.push $ Right (n, blk))
                SenderQCancel n blk -> modifyQ (Q.filter (filterPiece n blk))
                SenderOChoke -> do modifyQ (Q.filter filterAllPiece)
                                   modifyQ (Q.push $ Left Choke)
                SenderQRequestPrune n blk ->
                     modifyQ (Q.filter (filterRequest n blk))

rateUpdateEvent :: Process CF ST ()
rateUpdateEvent = {-# SCC "Peer.SendQ.rateUpd" #-} do
    l <- gets bytesTransferred
    bwc <- asks bandwidthCh
    liftIO . atomically $ writeTChan bwc l
    modify (\s -> s { bytesTransferred = 0 })

filterAllPiece :: Either Message (PieceNum, Block) -> Bool
filterAllPiece (Right _) = True
filterAllPiece (Left  _) = False

filterPiece :: PieceNum -> Block -> Either Message (PieceNum, Block) -> Bool
filterPiece n blk (Right (n1, blk1)) | n == n1 && blk == blk1 = False
                                     | otherwise              = True
filterPiece _ _   _                                           = True

filterRequest :: PieceNum -> Block -> Either Message (PieceNum, Block) -> Bool
filterRequest n blk (Left (Request n1 blk1)) | n == n1 && blk == blk1 = False
                                             | otherwise              = True
filterRequest _ _   _                                                 = True

modifyQ :: (Q.Queue (Either Message (PieceNum, Block)) ->
            Q.Queue (Either Message (PieceNum, Block)))
                    -> Process CF ST ()
modifyQ f = modify (\s -> s { outQueue = f $! outQueue s })

-- | Read a block from the filesystem for sending
readBlock :: PieceNum -> Block -> Process CF ST B.ByteString
readBlock pn blk = do
    v <- asks readBlockTV
    fch <- asks fsCh
    liftIO $ do
        atomically $ writeTChan fch (ReadBlock pn blk v)
        atomically $ takeTMVar v
