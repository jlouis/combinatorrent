-- | Peer proceeses
{-# LANGUAGE ScopedTypeVariables #-}
module Process.Peer (
    -- * Types
      PeerMessage(..)
    -- * Interface
    , peerChildren
    )
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.CML.Strict
import Control.Concurrent.STM

import Control.DeepSeq

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
import System.Timeout

import PeerTypes
import Process
import Process.FS
import Process.PieceMgr
import qualified Data.Queue as Q
import RateCalc as RC
import Process.Status
import Process.ChokeMgr (RateTVar)
import Supervisor
import Process.Timer as Timer
import Torrent
import Protocol.Wire

-- INTERFACE
----------------------------------------------------------------------

peerChildren :: Handle -> MgrChannel -> RateTVar -> PieceMgrChannel
             -> FSPChannel -> StatusChan -> PieceMap -> Int -> InfoHash
             -> IO Children
peerChildren handle pMgrC rtv pieceMgrC fsC statusC pm nPieces ih = do
    queueC <- channel
    senderC <- channel
    receiverC <- channel
    sendBWC <- channel
    return [Worker $ senderP handle senderC,
            Worker $ sendQueueP queueC senderC sendBWC,
            Worker $ receiverP handle receiverC,
            Worker $ peerP pMgrC rtv pieceMgrC fsC pm nPieces handle
                                queueC receiverC sendBWC statusC ih]

-- INTERNAL FUNCTIONS
----------------------------------------------------------------------
data SPCF = SPCF

instance Logging SPCF where
    logName _ = "Process.Peer.Sender"

-- | The raw sender process, it does nothing but send out what it syncs on.
senderP :: Handle -> Channel B.ByteString -> SupervisorChan -> IO ThreadId
senderP h ch supC = spawnP SPCF h (catchP (foreverP pgm)
                                              (do t <- liftIO $ myThreadId
                                                  syncP =<< (sendP supC $ IAmDying t)
                                                  liftIO $ hClose h))
  where
    pgm :: Process SPCF Handle ()
    pgm = {-# SCC "Peer.Sender" #-} do
        m <- liftIO $ timeout defaultTimeout s
        h <- get
        case m of
            Nothing -> putMsg (encodePacket KeepAlive)
            Just m  -> putMsg m
        liftIO $ hFlush h
    defaultTimeout = 120 * 1000000
    putMsg m = liftIO $ B.hPut h m
    s = sync $ receive ch (const True)

-- | Messages we can send to the Send Queue
data SendQueueMessage = SendQCancel PieceNum Block -- ^ Peer requested that we cancel a piece
                      | SendQMsg Message           -- ^ We want to send the Message to the peer
                      | SendOChoke                 -- ^ We want to choke the peer
                      | SendQRequestPrune PieceNum Block -- ^ Prune SendQueue of this (pn, blk) pair

instance NFData SendQueueMessage where
  rnf a = a `seq` ()

data SQCF = SQCF { sqInCh :: Channel SendQueueMessage
                 , sqOutCh :: Channel B.ByteString
                 , bandwidthCh :: BandwidthChannel
                 }

data SQST = SQST { outQueue :: Q.Queue Message
                 , bytesTransferred :: Integer
                 }

instance Logging SQCF where
    logName _ = "Process.Peer.SendQueue"

-- | sendQueue Process, simple version.
--   TODO: Split into fast and slow.
sendQueueP :: Channel SendQueueMessage -> Channel B.ByteString -> BandwidthChannel
           -> SupervisorChan
           -> IO ThreadId
sendQueueP inC outC bandwC supC = spawnP (SQCF inC outC bandwC) (SQST Q.empty 0)
        (catchP (foreverP pgm)
                (defaultStopHandler supC))
  where
    pgm :: Process SQCF SQST ()
    pgm = {-# SCC "Peer.SendQueue" #-} do
        q <- gets outQueue
        l <- gets bytesTransferred
        -- Gather together events which may trigger
        syncP =<< (chooseP $
            concat [if Q.isEmpty q then [] else [sendEvent],
                    if l > 0 then [rateUpdateEvent] else [],
                    [queueEvent]])
    rateUpdateEvent = {-# SCC "Peer.SendQ.rateUpd" #-} do
        l <- gets bytesTransferred
        ev <- sendPC bandwidthCh l
        wrapP ev (\() ->
            modify (\s -> s { bytesTransferred = 0 }))
    queueEvent = {-# SCC "Peer.SendQ.queueEvt" #-} do
        recvWrapPC sqInCh
                (\m -> case m of
                    SendQMsg msg -> do debugP "Queueing event for sending"
                                       modifyQ (Q.push msg)
                    SendQCancel n blk -> modifyQ (Q.filter (filterPiece n (blockOffset blk)))
                    SendOChoke -> do modifyQ (Q.filter filterAllPiece)
                                     modifyQ (Q.push Choke)
                    SendQRequestPrune n blk ->
                         modifyQ (Q.filter (filterRequest n blk)))
    modifyQ :: (Q.Queue Message -> Q.Queue Message) -> Process SQCF SQST ()
    modifyQ f = modify (\s -> s { outQueue = f (outQueue s) })
    sendEvent = {-# SCC "Peer.SendQ.sendEvt" #-} do
        Just (e, r) <- gets (Q.pop . outQueue)
        let bs = encodePacket e
        tEvt <- sendPC sqOutCh bs
        wrapP tEvt (\() -> do debugP "Dequeued event"
                              modify (\s -> s { outQueue = r,
                                                bytesTransferred =
                                                    bytesTransferred s + fromIntegral (B.length bs)}))
    filterAllPiece (Piece _ _ _) = True
    filterAllPiece _             = False
    filterPiece n off m =
        case m of Piece n off _ -> False
                  _             -> True
    filterRequest n blk m =
        case m of Request n blk -> False
                  _             -> True

data RPCF = RPCF { rpMsgCh :: Channel (Message, Integer) }

instance Logging RPCF where
    logName _ = "Process.Peer.Receiver"

receiverP :: Handle -> Channel (Message, Integer)
          -> SupervisorChan -> IO ThreadId
receiverP h ch supC = spawnP (RPCF ch) h
        (catchP (foreverP pgm)
               (defaultStopHandler supC))
  where
    pgm = readHeader
    readHeader = {-# SCC "Recv_readHeader" #-} do
        ch <- asks rpMsgCh
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
    conv bs = {-# SCC "Recv_conf" #-} do
        case G.runGet G.getWord32be bs of
          Left err -> do warningP $ "Incorrent parse in receiver, dying: " ++ show err
                         stopP
          Right i -> return i

data PCF = PCF { inCh :: Channel (Message, Integer)
               , outCh :: Channel SendQueueMessage
               , peerMgrCh :: MgrChannel
               , pieceMgrCh :: PieceMgrChannel
               , fsCh :: FSPChannel
               , peerCh :: PeerChannel
               , sendBWCh :: BandwidthChannel
               , timerCh :: Channel ()
               , statCh :: StatusChan
               , pieceMap :: PieceMap
               }

instance Logging PCF where
    logName _ = "Process.Peer"

data PST = PST { weChoke :: Bool -- ^ True if we are choking the peer
               , weInterested :: Bool -- ^ True if we are interested in the peer
               , blockQueue :: S.Set (PieceNum, Block) -- ^ Blocks queued at the peer
               , peerChoke :: Bool -- ^ Is the peer choking us? True if yes
               , peerInterested :: Bool -- ^ True if the peer is interested
               , peerPieces :: PS.PieceSet -- ^ List of pieces the peer has access to
               , upRate :: Rate -- ^ Upload rate towards the peer (estimated)
               , downRate :: Rate -- ^ Download rate from the peer (estimated)
               , runningEndgame :: Bool -- ^ True if we are in endgame
               }

peerP :: MgrChannel -> RateTVar -> PieceMgrChannel -> FSPChannel -> PieceMap -> Int -> Handle
         -> Channel SendQueueMessage -> Channel (Message, Integer) -> BandwidthChannel
         -> StatusChan -> InfoHash
         -> SupervisorChan -> IO ThreadId
peerP pMgrC rtv pieceMgrC fsC pm nPieces h outBound inBound sendBWC statC ih supC = do
    ch <- channel
    tch <- channel
    ct <- getCurrentTime
    pieceSet <- PS.new nPieces
    spawnP (PCF inBound outBound pMgrC pieceMgrC fsC ch sendBWC tch statC pm)
           (PST True False S.empty True False pieceSet (RC.new ct) (RC.new ct) False)
           (cleanupP startup (defaultStopHandler supC) cleanup)
  where startup = do
            tid <- liftIO $ myThreadId
            debugP "Syncing a connectBack"
            asks peerCh >>= (\ch -> sendPC peerMgrCh $ Connect ih tid ch) >>= syncP
            pieces <- getPiecesDone
            syncP =<< (sendPC outCh $ SendQMsg $ BitField (constructBitField nPieces pieces))
            -- Install the StatusP timer
            c <- asks timerCh
            Timer.register 5 () c
            foreverP (eventLoop tid)
        cleanup = do
            t <- liftIO myThreadId
            pieces <- gets peerPieces >>= PS.toList
            syncP =<< sendPC pieceMgrCh (PeerUnhave pieces)
            syncP =<< sendPC peerMgrCh (Disconnect t)

        getPiecesDone = do
            c <- liftIO $ channel
            syncP =<< (sendPC pieceMgrCh $ GetDone c)
            recvP c (const True) >>= syncP
        eventLoop tid = {-# SCC "Peer.Control" #-} do
            syncP =<< chooseP [peerMsgEvent, chokeMgrEvent, upRateEvent, timerEvent tid]
        chokeMgrEvent = do
            evt <- recvPC peerCh
            wrapP evt (\msg -> do
                debugP "ChokeMgrEvent"
                case msg of
                    PieceCompleted pn -> do
                        syncP =<< (sendPC outCh $ SendQMsg $ Have pn)
                    ChokePeer -> do choking <- gets weChoke
                                    when (not choking)
                                         (do syncP =<< sendPC outCh SendOChoke
                                             debugP "ChokePeer"
                                             modify (\s -> s {weChoke = True}))
                    UnchokePeer -> do choking <- gets weChoke
                                      when choking
                                           (do syncP =<< (sendPC outCh $ SendQMsg Unchoke)
                                               debugP "UnchokePeer"
                                               modify (\s -> s {weChoke = False}))
                    CancelBlock pn blk -> do
                        modify (\s -> s { blockQueue = S.delete (pn, blk) $ blockQueue s })
                        syncP =<< (sendPC outCh $ SendQRequestPrune pn blk))
        isASeeder = (== nPieces) <$> (gets peerPieces >>= PS.size)
        timerEvent mTid = do
            evt <- recvPC timerCh
            wrapP evt (\() -> do
                debugP "TimerEvent"
                tch <- asks timerCh
                Timer.register 5 () tch
                -- Tell the ChokeMgr about our progress
                ur <- gets upRate
                dr <- gets downRate
                t <- liftIO $ getCurrentTime
                let (up, nur) = RC.extractRate t ur
                    (down, ndr) = RC.extractRate t dr
                infoP $ "Peer has rates up/down: " ++ show up ++ "/" ++ show down
                i <- gets peerInterested
                seed <- isASeeder
                pchoke <- gets peerChoke
                liftIO . atomically $ do
                    q <- readTVar rtv
                    writeTVar rtv ((mTid, (up, down, i, seed, pchoke)) : q)
                -- Tell the Status Process about our progress
                let (upCnt, nuRate) = RC.extractCount $ nur
                    (downCnt, ndRate) = RC.extractCount $ ndr
                debugP $ "Sending peerStats: " ++ show upCnt ++ ", " ++ show downCnt
                (sendPC statCh $ PeerStat { peerInfoHash = ih
                                          , peerUploaded = upCnt
                                          , peerDownloaded = downCnt }) >>= syncP
                modify (\s -> s { upRate = nuRate, downRate = ndRate }))
        upRateEvent = do
            evt <- recvPC sendBWCh
            wrapP evt (\uploaded -> do
                modify (\s -> s { upRate = RC.update uploaded $ upRate s}))
        peerMsgEvent = do
            evt <- recvPC inCh
            wrapP evt (\(msg, sz) -> do
                modify (\s -> s { downRate = RC.update sz $ downRate s})
                case msg of
                  KeepAlive  -> return ()
                  Choke      -> do putbackBlocks
                                   modify (\s -> s { peerChoke = True })
                  Unchoke    -> do modify (\s -> s { peerChoke = False })
                                   fillBlocks
                  Interested -> modify (\s -> s { peerInterested = True })
                  NotInterested -> modify (\s -> s { peerInterested = False })
                  Have pn -> haveMsg pn
                  BitField bf -> bitfieldMsg bf
                  Request pn blk -> requestMsg pn blk
                  Piece n os bs -> do pieceMsg n os bs
                                      fillBlocks
                  Cancel pn blk -> cancelMsg pn blk
                  Port _ -> return ()) -- No DHT yet, silently ignore
        putbackBlocks = do
            blks <- gets blockQueue
            syncP =<< sendPC pieceMgrCh (PutbackBlocks (S.toList blks))
            modify (\s -> s { blockQueue = S.empty })
        haveMsg :: PieceNum -> Process PCF PST ()
        haveMsg pn = do
            pm <- asks pieceMap
            if M.member pn pm
                then do PS.insert pn =<< gets peerPieces
                        syncP =<< sendPC pieceMgrCh (PeerHave [pn])
                        considerInterest
                else do warningP "Unknown Piece"
                        stopP
        bitfieldMsg bf = do
            pieces <- gets peerPieces
            piecesNull <- PS.null pieces
            if piecesNull
                -- TODO: Don't trust the bitfield
                then do peerP <- createPeerPieces nPieces bf
                        modify (\s -> s { peerPieces = peerP })
                        peerLs <- PS.toList peerP
                        syncP =<< sendPC pieceMgrCh (PeerHave peerLs)
                        considerInterest
                else do infoP "Got out of band Bitfield request, dying"
                        stopP
        requestMsg :: PieceNum -> Block -> Process PCF PST ()
        requestMsg pn blk = do
            choking <- gets weChoke
            unless (choking)
                 (do
                    bs <- readBlock pn blk -- TODO: Pushdown to send process
                    syncP =<< sendPC outCh (SendQMsg $ Piece pn (blockOffset blk) bs))
        readBlock :: PieceNum -> Block -> Process PCF PST B.ByteString
        readBlock pn blk = do
            c <- liftIO $ channel
            syncP =<< sendPC fsCh (ReadBlock pn blk c)
            syncP =<< recvP c (const True)
        pieceMsg :: PieceNum -> Int -> B.ByteString -> Process PCF PST ()
        pieceMsg n os bs = do
            let sz = B.length bs
                blk = Block os sz
                e = (n, blk)
            q <- gets blockQueue
            when (S.member e q)
                (do storeBlock n (Block os sz) bs
                    modify (\s -> s { blockQueue = S.delete e (blockQueue s)}))
            -- When e is not a member, the piece may be stray, so ignore it.
            -- Perhaps print something here.
        cancelMsg n blk =
            syncP =<< sendPC outCh (SendQCancel n blk)
        considerInterest = do
            c <- liftIO channel
            pcs <- gets peerPieces
            syncP =<< sendPC pieceMgrCh (AskInterested pcs c)
            interested <- syncP =<< recvP c (const True)
            if interested
                then do modify (\s -> s { weInterested = True })
                        syncP =<< sendPC outCh (SendQMsg Interested)
                else modify (\s -> s { weInterested = False})
        fillBlocks = do
            choked <- gets peerChoke
            unless choked checkWatermark
        checkWatermark = do
            q <- gets blockQueue
            eg <- gets runningEndgame
            let sz = S.size q
                mark = if eg then endgameLoMark else loMark
            when (sz < mark)
                (do
                   toQueue <- grabBlocks (hiMark - sz)
                   debugP $ "Got " ++ show (length toQueue) ++ " blocks: " ++ show toQueue
                   queuePieces toQueue)
        queuePieces toQueue = do
            mapM_ pushPiece toQueue
            modify (\s -> s { blockQueue = S.union (blockQueue s) (S.fromList toQueue) })
        pushPiece (pn, blk) =
            syncP =<< sendPC outCh (SendQMsg $ Request pn blk)
        storeBlock n blk bs =
            syncP =<< sendPC pieceMgrCh (StoreBlock n blk bs)
        grabBlocks n = do
            c <- liftIO $ channel
            ps <- gets peerPieces
            syncP =<< sendPC pieceMgrCh (GrabBlocks n ps c)
            blks <- syncP =<< recvP c (const True)
            case blks of
                Leech blks -> return blks
                Endgame blks ->
                    modify (\s -> s { runningEndgame = True }) >> return blks
        loMark = 10
        endgameLoMark = 1
        hiMark = 15 -- These three values are chosen rather arbitrarily at the moment.

createPeerPieces :: MonadIO m => Int -> L.ByteString -> m PS.PieceSet
createPeerPieces nPieces =
    PS.fromList nPieces . map fromIntegral . concat . decodeBytes 0 . L.unpack
  where decodeByte :: Int -> Word8 -> [Maybe Int]
        decodeByte soFar w =
            let dBit n = if testBit w (7-n)
                           then Just (n+soFar)
                           else Nothing
            in fmap dBit [0..7]
        decodeBytes _ [] = []
        decodeBytes soFar (w : ws) = catMaybes (decodeByte soFar w) : decodeBytes (soFar + 8) ws
