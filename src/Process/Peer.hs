-- | Peer proceeses
{-# LANGUAGE ScopedTypeVariables #-}
module Process.Peer (
    -- * Types
      PeerMessage(..)
    -- * Interface
    , Process.Peer.start
    )
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception


import Control.Monad.State
import Control.Monad.Reader

import Prelude hiding (catch, log)

import Data.Array
import Data.Bits
import qualified Data.ByteString as B

import qualified Data.PieceSet as PS
import Data.Maybe

import Data.Set as S hiding (map)
import Data.Time.Clock
import Data.Word

import Network.Socket hiding (KeepAlive)

import Channels
import Process
import Process.FS
import Process.PieceMgr
import RateCalc as RC
import Process.Status
import Process.ChokeMgr (RateTVar)
import Process.Timer
import Supervisor
import Torrent
import Protocol.Wire

import qualified Process.Peer.Sender as Sender
import qualified Process.Peer.SenderQ as SenderQ
import qualified Process.Peer.Receiver as Receiver

-- INTERFACE
----------------------------------------------------------------------

start :: Socket -> MgrChannel -> RateTVar -> PieceMgrChannel
             -> FSPChannel -> TVar [PStat] -> PieceMap -> Int -> InfoHash
             -> IO Children
start s pMgrC rtv pieceMgrC fsC stv pm nPieces ih = do
    queueC <- newTChanIO
    senderMV <- newEmptyTMVarIO
    receiverC <- newTChanIO
    sendBWC <- newTChanIO
    return [Worker $ Sender.start s senderMV,
            Worker $ SenderQ.start queueC senderMV sendBWC fsC,
            Worker $ Receiver.start s receiverC,
            Worker $ peerP pMgrC rtv pieceMgrC pm nPieces
                                queueC receiverC sendBWC stv ih]

-- INTERNAL FUNCTIONS
----------------------------------------------------------------------

data CF = CF { inCh :: TChan (Message, Integer)
             , outCh :: TChan SenderQ.SenderQMsg
             , peerMgrCh :: MgrChannel
             , pieceMgrCh :: PieceMgrChannel
             , peerCh :: PeerChannel
             , sendBWCh :: BandwidthChannel
             , timerCh :: TChan ()
             , statTV :: TVar [PStat]
             , rateTV :: RateTVar
             , pcInfoHash :: InfoHash
             , pieceMap :: !PieceMap
             , piecesDoneTV :: TMVar [PieceNum]
             , interestTV :: TMVar Bool
             , grabBlockTV :: TMVar Blocks
             }

instance Logging CF where
    logName _ = "Process.Peer"

data ST = ST { weChoke        :: !Bool -- ^ True if we are choking the peer
             , weInterested   :: !Bool -- ^ True if we are interested in the peer
             , blockQueue     :: !(S.Set (PieceNum, Block)) -- ^ Blocks queued at the peer
             , peerChoke      :: !Bool -- ^ Is the peer choking us? True if yes
             , peerInterested :: !Bool -- ^ True if the peer is interested
             , peerPieces     :: !(PS.PieceSet) -- ^ List of pieces the peer has access to
             , missingPieces  :: !Int -- ^ Tracks the number of pieces the peer misses before seeding
             , upRate         :: !Rate -- ^ Upload rate towards the peer (estimated)
             , downRate       :: !Rate -- ^ Download rate from the peer (estimated)
             , runningEndgame :: !Bool -- ^ True if we are in endgame
             , lastMessage    :: !Int
             }


peerP :: MgrChannel -> RateTVar -> PieceMgrChannel -> PieceMap -> Int
         -> TChan SenderQ.SenderQMsg -> TChan (Message, Integer) -> BandwidthChannel
         -> TVar [PStat] -> InfoHash
         -> SupervisorChannel -> IO ThreadId
peerP pMgrC rtv pieceMgrC pm nPieces outBound inBound sendBWC stv ih supC = do
    ch <- newTChanIO
    tch <- newTChanIO
    ct <- getCurrentTime
    pdtmv <- newEmptyTMVarIO
    intmv <- newEmptyTMVarIO
    gbtmv <- newEmptyTMVarIO
    pieceSet <- PS.new nPieces
    spawnP (CF inBound outBound pMgrC pieceMgrC ch sendBWC tch stv rtv ih pm
                    pdtmv intmv gbtmv)
           (ST True False S.empty True False pieceSet nPieces (RC.new ct) (RC.new ct) False 0)
                       ({-# SCC "PeerControl" #-}
                            cleanupP (startup nPieces) (defaultStopHandler supC) cleanup)

startup :: Int -> Process CF ST ()
startup nPieces = do
    tid <- liftIO $ myThreadId
    ch <- asks peerCh
    pmc <- asks peerMgrCh
    ih <- asks pcInfoHash
    liftIO . atomically $ writeTChan pmc $ Connect ih tid ch
    pieces <- getPiecesDone
    outChan $ SenderQ.SenderQM $ BitField (constructBitField nPieces pieces)
    -- Install the StatusP timer
    c <- asks timerCh
    _ <- registerSTM 5 c ()
    eventLoop

cleanup :: Process CF ST ()
cleanup = do
    t <- liftIO myThreadId
    pieces <- gets peerPieces >>= PS.toList
    ch2 <- asks peerMgrCh
    msgPieceMgr (PeerUnhave pieces)
    liftIO . atomically $ writeTChan ch2 (Disconnect t)

readOp :: Process CF ST Operation
readOp = do
    inb <- asks inCh
    chk <- asks peerCh
    tch <- asks timerCh
    bwc <- asks sendBWCh
    liftIO . atomically $
       (readTChan inb >>= return . PeerMsgEvt) `orElse`
       (readTChan chk >>= return . ChokeMgrEvt) `orElse`
       (readTChan tch >>  return TimerEvent) `orElse`
       (readTChan bwc >>= return . UpRateEvent)

eventLoop :: Process CF ST ()
eventLoop = do
    op <- readOp
    case op of
        PeerMsgEvt (m, sz) -> peerMsg m sz
        ChokeMgrEvt m      -> chokeMsg m
        UpRateEvent up     -> do s <- get
                                 u <- return $ RC.update up $ upRate s
                                 put $! s { upRate = u }
        TimerEvent         -> timerTick
    eventLoop

data Operation = PeerMsgEvt (Message, Integer)
               | ChokeMgrEvt PeerMessage
               | TimerEvent
               | UpRateEvent Integer


-- | Return a list of pieces which are currently done by us
getPiecesDone :: Process CF ST [PieceNum]
getPiecesDone = do
    c  <- asks piecesDoneTV
    msgPieceMgr (GetDone c)
    liftIO $ do atomically $ takeTMVar c

-- | Process an event from the Choke Manager
chokeMsg :: PeerMessage -> Process CF ST ()
chokeMsg msg = do
   case msg of
       PieceCompleted pn -> outChan $ SenderQ.SenderQM $ Have pn
       ChokePeer -> do choking <- gets weChoke
                       when (not choking)
                            (do outChan $ SenderQ.SenderOChoke
                                modify (\s -> s {weChoke = True}))
       UnchokePeer -> do choking <- gets weChoke
                         when choking
                              (do outChan $ SenderQ.SenderQM Unchoke
                                  modify (\s -> s {weChoke = False}))
       CancelBlock pn blk -> do
           modify (\s -> s { blockQueue = S.delete (pn, blk) $ blockQueue s })
           outChan $ SenderQ.SenderQRequestPrune pn blk

processLastMessage :: Process CF ST ()
processLastMessage = do
    lm <- gets lastMessage
    if lm >= 24
        then do outChan $ SenderQ.SenderQM KeepAlive
        else let inc = succ lm
             in inc `seq` modify (\st -> st { lastMessage = inc })

-- A Timer event handles a number of different status updates. One towards the
-- Choke Manager so it has a information about whom to choke and unchoke - and
-- one towards the status process to keep track of uploaded and downloaded
-- stuff.
timerTick :: Process CF ST ()
timerTick = do
   mTid <- liftIO myThreadId
   processLastMessage
   tch <- asks timerCh
   _ <- registerSTM 5 tch ()
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
   rtv <- asks rateTV
   liftIO . atomically $ do
       q <- readTVar rtv
       writeTVar rtv ((mTid, (up, down, i, seed, pchoke)) : q)
   -- Tell the Status Process about our progress
   let (upCnt, nuRate) = RC.extractCount $ nur
       (downCnt, ndRate) = RC.extractCount $ ndr
   stv <- asks statTV
   ih <- asks pcInfoHash
   liftIO .atomically $ do
       q <- readTVar stv
       writeTVar stv (PStat { pInfoHash = ih
                            , pUploaded = upCnt
                            , pDownloaded = downCnt } : q)
   modify (\s -> s { upRate = nuRate, downRate = ndRate })


-- | Process an Message from the peer in the other end of the socket.
peerMsg :: Message -> Integer -> Process CF ST ()
peerMsg msg sz = do
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
     Port _ -> return () -- No DHT yet, silently ignore

-- | Put back blocks for other peer processes to grab. This is done whenever
-- the peer chokes us, or if we die by an unknown cause.
putbackBlocks :: Process CF ST ()
putbackBlocks = do
    blks <- gets blockQueue
    msgPieceMgr (PutbackBlocks (S.toList blks))
    modify (\s -> s { blockQueue = S.empty })

-- | Process a HAVE message from the peer. Note we also update interest as a side effect
haveMsg :: PieceNum -> Process CF ST ()
haveMsg pn = do
    pm <- asks pieceMap
    let (lo, hi) = bounds pm
    if pn >= lo && pn <= hi
        then do PS.insert pn =<< gets peerPieces
                msgPieceMgr (PeerHave [pn])
                decMissingCounter 1
                considerInterest
        else do warningP "Unknown Piece"
                stopP

-- True if the peer is a seeder
isASeeder :: Process CF ST Bool
isASeeder = do sdr <- gets missingPieces
               sdr `deepseq` (return $! sdr == 0)

-- Decrease the counter of missing pieces for the peer
decMissingCounter :: Int -> Process CF ST ()
decMissingCounter n = do
    modify (\s -> s { missingPieces = missingPieces s - n})
    m <- gets missingPieces
    when (m == 0) assertSeeder

-- Assert that the peer is a seeder
assertSeeder :: Process CF ST ()
assertSeeder = do
    ok <- liftM2 (==) (gets peerPieces >>= PS.size) (succ . snd . bounds <$> asks pieceMap)
    assert ok (return ())

-- | Process a BITFIELD message from the peer. Side effect: Consider Interest.
bitfieldMsg :: BitField -> Process CF ST ()
bitfieldMsg bf = do
    pieces <- gets peerPieces
    piecesNull <- PS.null pieces
    if piecesNull
        -- TODO: Don't trust the bitfield
        then do nPieces <- succ . snd . bounds <$> asks pieceMap
                pp <- createPeerPieces nPieces bf
                modify (\s -> s { peerPieces = pp })
                peerLs <- PS.toList pp
                msgPieceMgr (PeerHave peerLs)
                decMissingCounter (length peerLs)
                considerInterest
        else do infoP "Got out of band Bitfield request, dying"
                stopP

-- | Process a request message from the Peer
requestMsg :: PieceNum -> Block -> Process CF ST ()
requestMsg pn blk = do
    choking <- gets weChoke
    unless (choking)
         (outChan $ SenderQ.SenderQPiece pn blk)

-- | Handle a Piece Message incoming from the peer
pieceMsg :: PieceNum -> Int -> B.ByteString -> Process CF ST ()
pieceMsg n os bs = do
    let sz = B.length bs
        blk = Block os sz
        e = (n, blk)
    q <- gets blockQueue
    -- When e is not a member, the piece may be stray, so ignore it.
    -- Perhaps print something here.
    when (S.member e q)
        (do storeBlock n blk bs
            bq <- gets blockQueue >>= return . S.delete e
            bq `deepseq` modify (\s -> s { blockQueue = bq }))

-- | Handle a cancel message from the peer
cancelMsg :: PieceNum -> Block -> Process CF ST ()
cancelMsg n blk = outChan $ SenderQ.SenderQCancel n blk

-- | Update our interest state based on the pieces the peer has.
--   Obvious optimization: Do less work, there is no need to consider all pieces most of the time
considerInterest :: Process CF ST ()
considerInterest = do
    c <- asks interestTV
    pcs <- gets peerPieces
    msgPieceMgr (AskInterested pcs c)
    interested <- liftIO $ do atomically $ takeTMVar c
    if interested
        then do modify (\s -> s { weInterested = True })
                outChan $ SenderQ.SenderQM Interested
        else modify (\s -> s { weInterested = False})

-- | Try to fill up the block queue at the peer. The reason we pipeline a
-- number of blocks is to get around the line delay present on the internet.
fillBlocks :: Process CF ST ()
fillBlocks = do
    choked <- gets peerChoke
    unless choked checkWatermark

-- | check the current Watermark level. If we are below the lower one, then
-- fill till the upper one. This in turn keeps the pipeline of pieces full as
-- long as the peer is interested in talking to us.
-- TODO: Decide on a queue size based on the current download rate.
checkWatermark :: Process CF ST ()
checkWatermark = do
    q <- gets blockQueue
    eg <- gets runningEndgame
    let sz = S.size q
        mark = if eg then endgameLoMark else loMark
    when (sz < mark)
        (do
           toQueue <- grabBlocks (hiMark - sz)
           queuePieces toQueue)

-- These three values are chosen rather arbitrarily at the moment.
loMark :: Int
loMark = 5

hiMark :: Int
hiMark = 25

-- Low mark when running in endgame mode
endgameLoMark :: Int
endgameLoMark = 1


-- | Queue up pieces for retrieval at the Peer
queuePieces :: [(PieceNum, Block)] -> Process CF ST ()
queuePieces toQueue = do
    mapM_ (uncurry pushRequest) toQueue
    modify (\s -> s { blockQueue = S.union (blockQueue s) (S.fromList toQueue) })

-- | Push a request to the peer so he can send it to us
pushRequest :: PieceNum -> Block -> Process CF ST ()
pushRequest pn blk = outChan $ SenderQ.SenderQM $ Request pn blk

-- | Tell the PieceManager to store the given block
storeBlock :: PieceNum -> Block -> B.ByteString -> Process CF ST ()
storeBlock n blk bs = msgPieceMgr (StoreBlock n blk bs)

-- | The call @grabBlocks n@ will attempt to grab (up to) @n@ blocks from the
-- piece Manager for request at the peer.
grabBlocks :: Int -> Process CF ST [(PieceNum, Block)]
grabBlocks n = do
    c <- asks grabBlockTV
    ps <- gets peerPieces
    msgPieceMgr (GrabBlocks n ps c)
    blks <- liftIO $ do atomically $ takeTMVar c
    case blks of
        Leech bs -> return bs
        Endgame bs ->
            modify (\s -> s { runningEndgame = True }) >> return bs


createPeerPieces :: MonadIO m => Int -> B.ByteString -> m PS.PieceSet
createPeerPieces nPieces =
    PS.fromList nPieces . map fromIntegral . concat . decodeBytes 0 . B.unpack
  where decodeByte :: Int -> Word8 -> [Maybe Int]
        decodeByte soFar w =
            let dBit n = if testBit w (7-n)
                           then Just (n+soFar)
                           else Nothing
            in fmap dBit [0..7]
        decodeBytes _ [] = []
        decodeBytes soFar (w : ws) = catMaybes (decodeByte soFar w) : decodeBytes (soFar + 8) ws

-- | Send a message on a chan from the process queue
outChan :: SenderQ.SenderQMsg -> Process CF ST ()
outChan qm = do
    modify (\st -> st { lastMessage = 0 })
    ch <- asks outCh
    liftIO . atomically $ writeTChan ch qm

msgPieceMgr :: PieceMgrMsg -> Process CF ST ()
msgPieceMgr m = do
   pmc <- asks pieceMgrCh
   {-# SCC "Channel_Write" #-} liftIO . atomically $ writeTChan pmc m

