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
import Data.List (foldl')

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
             , peerCtlCh :: TChan MsgTy
             , readBlockTV :: TMVar B.ByteString
             , fsCh        :: FSPChannel
             , fastExtension :: Bool
             }

data ST = ST { outQueue         :: !(Q.Queue (Either Message (PieceNum, Block)))
             , bytesTransferred :: !Int
             }

instance Logging CF where
    logName _ = "Process.Peer.SendQueue"

-- | sendQueue Process, simple version.
--   TODO: Split into fast and slow.
start :: [Capabilities] -> TChan SenderQMsg -> TMVar L.ByteString -> TChan MsgTy
      -> FSPChannel -> SupervisorChannel -> IO ThreadId
start caps inC outC bandwC fspC supC = do
    rbtv <- liftIO newEmptyTMVarIO
    spawnP (CF inC outC bandwC rbtv fspC
                (Fast `elem` caps)) (ST Q.empty 0)
        ({-# SCC "SenderQ" #-} catchP pgm
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
                SenderQCancel n blk -> do
                    fe <- asks fastExtension
                    if fe
                        then do
                            piece <- partitionQ (pickPiece n blk)
                            case piece of
                                [] -> return () -- Piece must have been sent
                                [_] -> modifyQ (Q.push (Left $ RejectRequest n blk))
                                ps -> fail $ "Impossible case, SenderQCancel " ++ show (length ps)
                        else modifyQ (Q.filter (filterPiece n blk))
                SenderOChoke -> do
                    fe <- asks fastExtension
                    if fe
                        then do
                            -- In the fast extension, we explicitly reject all pieces
                            pieces <- partitionQ filterAllPiece
                            modifyQ (Q.push $ Left Choke)
                            let rejects = map (\(Right (pn, blk)) -> Left $ RejectRequest pn blk)
                                              pieces
                            modifyQ (flip (foldl' (flip Q.push)) rejects)
                        else do modifyQ (Q.filter filterAllPiece)
                                modifyQ (Q.push $ Left Choke)
                SenderQRequestPrune n blk -> do
                    fe <- asks fastExtension
                    piece <- partitionQ (pickRequest n blk)
                    case piece of
                      [] -> modifyQ (Q.push (Left $ Cancel n blk)) -- Request must have been sent
                      [_] -> if fe
                                then modifyQ -- This is a hack for now
                                       (Q.push (Left $ Cancel n blk) .
                                        Q.push (Left $ Request n blk))
                                else return ()
                      ps -> fail $ "Impossible case, SenderQRequestPrune "
                                ++ show ps ++ ", " ++ show fe
    pgm

rateUpdateEvent :: Process CF ST ()
rateUpdateEvent = {-# SCC "Peer.SendQ.rateUpd" #-} do
    l <- gets bytesTransferred
    bwc <- asks peerCtlCh
    liftIO . atomically $ writeTChan bwc (FromSenderQ l)
    modify (\s -> s { bytesTransferred = 0 })

-- The type of the Outgoing queue
type OutQT = Either Message (PieceNum, Block)

filterAllPiece :: OutQT -> Bool
filterAllPiece (Right _) = True
filterAllPiece (Left  _) = False

pickPiece :: PieceNum -> Block -> OutQT -> Bool
pickPiece n blk (Right (n1, blk1)) | n == n1 && blk == blk1 = True
pickPiece _ _   _                                           = False

filterPiece :: PieceNum -> Block -> OutQT -> Bool
filterPiece n blk (Right (n1, blk1)) | n == n1 && blk == blk1 = False
                                     | otherwise              = True
filterPiece _ _   _                                           = True

pickRequest :: PieceNum -> Block -> OutQT -> Bool
pickRequest n blk (Left (Request n1 blk1)) | n == n1 && blk == blk1 = True
pickRequest _ _   _                                                 = False

modifyQ :: (Q.Queue (OutQT) ->
            Q.Queue (OutQT))
                    -> Process CF ST ()
modifyQ f = modify (\s -> s { outQueue = f $! outQueue s })

partitionQ :: (OutQT -> Bool) -> Process CF ST [OutQT]
partitionQ p = do
    s <- get
    let (as, nq) = Q.partition p $ outQueue s
    put $! s { outQueue = nq }
    return as

-- | Read a block from the filesystem for sending
readBlock :: PieceNum -> Block -> Process CF ST B.ByteString
readBlock pn blk = do
    v <- asks readBlockTV
    fch <- asks fsCh
    liftIO $ do
        atomically $ writeTChan fch (ReadBlock pn blk v)
        atomically $ takeTMVar v
