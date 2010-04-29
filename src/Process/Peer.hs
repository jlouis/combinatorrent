-- | Peer proceeses
{-# LANGUAGE ScopedTypeVariables #-}
module Process.Peer (
    -- * Types
      PeerMessage(..)
    -- * Interface
    , Process.Peer.start
    -- * Tests
    , Process.Peer.testSuite
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
import Data.Function (on)

import qualified Data.PieceSet as PS
import Data.Maybe
import Data.Monoid(Monoid(..), Last(..))

import Data.Set as S hiding (map)
import Data.Time.Clock
import Data.Word

import Network.Socket hiding (KeepAlive)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Path, Test, assert)

import Channels
import Digest
import Process
import Process.FS
import Process.PieceMgr
import RateCalc as RC
import Process.Status
import qualified Process.ChokeMgr as ChokeMgr (RateTVar, PeerRateInfo(..))
import Process.Timer
import Supervisor
import Torrent
import Protocol.Wire

import qualified Process.Peer.Sender as Sender
import qualified Process.Peer.SenderQ as SenderQ
import qualified Process.Peer.Receiver as Receiver

-- INTERFACE
----------------------------------------------------------------------

start :: Socket -> [Capabilities] -> MgrChannel -> ChokeMgr.RateTVar -> PieceMgrChannel
             -> FSPChannel -> TVar [PStat] -> PieceMap -> Int -> InfoHash
             -> IO Children
start s caps pMgrC rtv pieceMgrC fsC stv pm nPieces ih = do
    queueC <- newTChanIO
    senderMV <- newEmptyTMVarIO
    receiverC <- newTChanIO
    sendBWC <- newTChanIO
    return [Worker $ Sender.start s senderMV,
            Worker $ SenderQ.start caps queueC senderMV sendBWC fsC,
            Worker $ Receiver.start s receiverC,
            Worker $ peerP caps pMgrC rtv pieceMgrC pm nPieces
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
             , rateTV :: ChokeMgr.RateTVar
             , pcInfoHash :: InfoHash
             , pieceMap :: !PieceMap
             , piecesDoneTV :: TMVar [PieceNum]
             , interestTV :: TMVar Bool
             , grabBlockTV :: TMVar Blocks
             , extConf :: ExtensionConfig
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

data ExtensionConfig = ExtensionConfig
        { handleHaveAll :: Last (Process CF ST ())
        , handleHaveNone :: Last (Process CF ST ())
        , handleBitfield :: Last (BitField -> Process CF ST ())
        , handleSuggest :: Last (PieceNum -> Process CF ST ())
        , handleAllowedFast :: Last (PieceNum -> Process CF ST ())
        , handleRejectRequest :: Last (PieceNum -> Block -> Process CF ST ())
        , handleExtendedMsg :: Last (Int -> B.ByteString -> Process CF ST ())
        , handleRequestMsg :: Last (PieceNum -> Block -> Process CF ST ())
        , handleChokeMsg   :: Last (Process CF ST ())
        , handleCancelMsg  :: Last (PieceNum -> Block -> Process CF ST ())
        , handlePieceMsg   :: Last (PieceNum -> Int -> B.ByteString -> Process CF ST ())
        }

emptyExtensionConfig :: ExtensionConfig
emptyExtensionConfig = ExtensionConfig
    mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty mempty

appendEConfig :: ExtensionConfig -> ExtensionConfig -> ExtensionConfig
appendEConfig a b =
    ExtensionConfig {
        handleHaveAll       = app handleHaveAll a b
      , handleHaveNone      = app handleHaveNone a b
      , handleBitfield      = app handleBitfield a b
      , handleSuggest       = app handleSuggest a b
      , handleAllowedFast   = app handleAllowedFast a b
      , handleRejectRequest = app handleRejectRequest a b
      , handleExtendedMsg   = app handleExtendedMsg a b
      , handleRequestMsg    = app handleRequestMsg a b
      , handleChokeMsg      = app handleChokeMsg a b
      , handleCancelMsg     = app handleCancelMsg a b
      , handlePieceMsg      = app handlePieceMsg a b
    }
 where app f = mappend `on` f

instance Monoid ExtensionConfig where
    mempty = emptyExtensionConfig
    mappend = appendEConfig

-- | Constructor for 'Last' values.
ljust :: a -> Last a
ljust = Last . Just

-- | Deconstructor for 'Last' values.
fromLJ :: (ExtensionConfig -> Last a)    -- ^ Field to access.
       -> ExtensionConfig                -- ^ Default to use.
       -> a
fromLJ f cfg = case f cfg of
                 Last Nothing  -> fromLJ f extensionBase
                 Last (Just a) -> a

extensionBase :: ExtensionConfig
extensionBase = ExtensionConfig
                 (ljust errorHaveAll)
                 (ljust errorHaveNone)
                 (ljust bitfieldMsg)
                 (ljust errorSuggest)
                 (ljust errorAllowedFast)
                 (ljust errorRejectRequest)
                 (ljust errorExtendedMsg)
                 (ljust requestMsg)
                 (ljust chokeMsg)
                 (ljust cancelBlock)
                 (ljust pieceMsg)

fastExtension :: ExtensionConfig
fastExtension = ExtensionConfig
                    (ljust haveAllMsg)
                    (ljust haveNoneMsg)
                    (ljust bitfieldMsg)
                    (ljust ignoreSuggest)
                    (ljust ignoreAllowedFast)
                    (ljust rejectMsg)
                    mempty
                    (ljust requestFastMsg)
                    (ljust fastChokeMsg)
                    (ljust fastCancelBlock)
                    (ljust fastPieceMsg)

ignoreSuggest :: PieceNum -> Process CF ST ()
ignoreSuggest _ = debugP "Ignoring SUGGEST message"

ignoreAllowedFast :: PieceNum -> Process CF ST ()
ignoreAllowedFast _ =  debugP "Ignoring ALLOWEDFAST message"

errorHaveAll :: Process CF ST ()
errorHaveAll = do
    errorP "Received a HAVEALL, but the extension is not enabled"
    stopP

errorHaveNone :: Process CF ST ()
errorHaveNone = do
    errorP "Received a HAVENONE, but the extension is not enabled"
    stopP

errorSuggest :: PieceNum -> Process CF ST ()
errorSuggest _ = do
    errorP "Received a SUGGEST PIECE, but the extension is not enabled"
    stopP

errorAllowedFast :: PieceNum -> Process CF ST ()
errorAllowedFast _ = do
    errorP "Received a ALLOWEDFAST, but the extension is not enabled"
    stopP

errorRejectRequest :: PieceNum -> Block -> Process CF ST ()
errorRejectRequest _ _ = do
    errorP "Received a REJECT REQUEST, but the extension is not enabled"
    stopP

errorExtendedMsg :: Int -> B.ByteString -> Process CF ST ()
errorExtendedMsg _ _ = do
    errorP "Received an EXTENDED MESSAGE, but the extension is not enabled"
    stopP

peerP :: [Capabilities] -> MgrChannel -> ChokeMgr.RateTVar -> PieceMgrChannel -> PieceMap -> Int
         -> TChan SenderQ.SenderQMsg -> TChan (Message, Integer) -> BandwidthChannel
         -> TVar [PStat] -> InfoHash
         -> SupervisorChannel -> IO ThreadId
peerP caps pMgrC rtv pieceMgrC pm nPieces outBound inBound sendBWC stv ih supC = do
    ch <- newTChanIO
    tch <- newTChanIO
    ct <- getCurrentTime
    pdtmv <- newEmptyTMVarIO
    intmv <- newEmptyTMVarIO
    gbtmv <- newEmptyTMVarIO
    pieceSet <- PS.new nPieces
    let cs = configCapabilities caps
    spawnP (CF inBound outBound pMgrC pieceMgrC ch sendBWC tch stv rtv ih pm
                    pdtmv intmv gbtmv cs)
           (ST True False S.empty True False pieceSet nPieces (RC.new ct) (RC.new ct) False 0)
                       ({-# SCC "PeerControl" #-}
                            cleanupP (startup nPieces) (defaultStopHandler supC) cleanup)

configCapabilities :: [Capabilities] -> ExtensionConfig
configCapabilities caps =
    mconcat [mempty, if Fast `elem` caps then fastExtension else mempty]

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
        ChokeMgrEvt m      -> chokeMgrMsg m
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
chokeMgrMsg :: PeerMessage -> Process CF ST ()
chokeMgrMsg msg = do
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
            cf <- asks extConf
            fromLJ handleCancelMsg cf pn blk

cancelBlock :: PieceNum -> Block -> Process CF ST ()
cancelBlock pn blk = do
    s <- get
    put $! s { blockQueue = S.delete (pn, blk) $ blockQueue s }
    outChan $ SenderQ.SenderQRequestPrune pn blk

fastCancelBlock :: PieceNum -> Block -> Process CF ST ()
fastCancelBlock pn blk = do
    bq <- gets blockQueue
    when (S.member (pn, blk) bq)
        $ outChan $ SenderQ.SenderQRequestPrune pn blk

checkKeepAlive :: Process CF ST ()
checkKeepAlive = do
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
   checkKeepAlive
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
       writeTVar rtv ((mTid, ChokeMgr.PRI {
                                   ChokeMgr.peerUpRate = up,
                                   ChokeMgr.peerDownRate = down,
                                   ChokeMgr.peerInterested = i,
                                   ChokeMgr.peerSeeding = seed,
                                   ChokeMgr.peerChokingUs = pchoke }) : q)
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


chokeMsg :: Process CF ST ()
chokeMsg = do
    putbackBlocks
    s <- get
    put $! s { peerChoke = True }

fastChokeMsg :: Process CF ST ()
fastChokeMsg = do
    s <- get
    put $! s { peerChoke = True}

unchokeMsg :: Process CF ST ()
unchokeMsg = do
    s <- get
    put $! s { peerChoke = False }
    fillBlocks

-- | Process an Message from the peer in the other end of the socket.
peerMsg :: Message -> Integer -> Process CF ST ()
peerMsg msg sz = do
   modify (\s -> s { downRate = RC.update sz $ downRate s})
   case msg of
     KeepAlive  -> return ()
     Choke      -> asks extConf >>= fromLJ handleChokeMsg
     Unchoke    -> unchokeMsg
     Interested -> modify (\s -> s { peerInterested = True })
     NotInterested -> modify (\s -> s { peerInterested = False })
     Have pn -> haveMsg pn
     BitField bf -> bitfieldMsg bf
     Request pn blk -> do cf <- asks extConf
                          fromLJ handleRequestMsg cf pn blk
     Piece n os bs -> do cf <- asks extConf
                         fromLJ handlePieceMsg cf n os bs
                         fillBlocks
     Cancel pn blk -> cancelMsg pn blk
     Port _ -> return () -- No DHT yet, silently ignore
     HaveAll -> fromLJ handleHaveAll =<< asks extConf
     HaveNone -> fromLJ handleHaveNone =<< asks extConf
     Suggest pn -> do cf <- asks extConf
                      fromLJ handleSuggest cf pn
     AllowedFast pn -> do cf <- asks extConf
                          fromLJ handleAllowedFast cf pn
     RejectRequest pn blk -> do cf <- asks extConf
                                fromLJ handleRejectRequest cf pn blk
     ExtendedMsg idx bs -> do cf <- asks extConf
                              fromLJ handleExtendedMsg cf idx bs

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
                fillBlocks
        else do warningP "Unknown Piece"
                stopP

-- True if the peer is a seeder
isASeeder :: Process CF ST Bool
isASeeder = do sdr <- gets missingPieces
               return $! sdr == 0

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

haveAllNoneMsg :: String -> Bool -> Process CF ST ()
haveAllNoneMsg ty a = do
    pieces <- gets peerPieces
    piecesNull <- PS.null pieces
    if piecesNull
        then do nPieces <- succ . snd . bounds <$> asks pieceMap
                pp <- createAllPieces nPieces a
                modify (\s -> s { peerPieces = pp})
                peerLs <- PS.toList pp
                msgPieceMgr (PeerHave peerLs)
                decMissingCounter (length peerLs)
                considerInterest
        else do infoP $ "Got out of band " ++ ty ++ " request, dying"
                stopP

haveNoneMsg :: Process CF ST ()
haveNoneMsg = haveAllNoneMsg "HaveNone" False

haveAllMsg :: Process CF ST ()
haveAllMsg = haveAllNoneMsg "HaveAll" True


-- | Process a request message from the Peer
requestMsg :: PieceNum -> Block -> Process CF ST ()
requestMsg pn blk = do
    choking <- gets weChoke
    unless (choking)
         (outChan $ SenderQ.SenderQPiece pn blk)

requestFastMsg :: PieceNum -> Block -> Process CF ST ()
requestFastMsg pn blk = do
    choking <- gets weChoke
    if choking
        then outChan $ SenderQ.SenderQM (RejectRequest pn blk)
        else outChan $ SenderQ.SenderQPiece pn blk

-- | Handle a Piece Message incoming from the peer
pieceMsg :: PieceNum -> Int -> B.ByteString -> Process CF ST ()
pieceMsg pn offs bs = pieceMsg' pn offs bs >> return ()

fastPieceMsg :: PieceNum -> Int -> B.ByteString -> Process CF ST ()
fastPieceMsg pn offs bs = do
    r <- pieceMsg' pn offs bs
    unless r
        (do infoP "Peer sent out-of-band piece we did not request, closing"
            stopP)

pieceMsg' :: PieceNum -> Int -> B.ByteString -> Process CF ST Bool
pieceMsg' n os bs = do
    let sz = B.length bs
        blk = Block os sz
        e = (n, blk)
    q <- gets blockQueue
    -- When e is not a member, the piece may be stray, so ignore it.
    -- Perhaps print something here.
    if S.member e q
        then do storeBlock n blk bs
                bq <- gets blockQueue >>= return . S.delete e
                s <- get
                bq `deepseq` put $! s { blockQueue = bq }
                return True
        else return False

rejectMsg :: PieceNum -> Block -> Process CF ST ()
rejectMsg pn blk = do
    let e = (pn, blk)
    q <- gets blockQueue
    if S.member e q
        then do
            msgPieceMgr (PutbackBlocks [e])
            s <- get
            put $! s { blockQueue = S.delete e q }
        else do
            infoP "Peer rejected piece/block we never requested, stopping"
            stopP

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
        else do modify (\s -> s { weInterested = False})
                outChan $ SenderQ.SenderQM NotInterested

-- | Try to fill up the block queue at the peer. The reason we pipeline a
-- number of blocks is to get around the line delay present on the internet.
fillBlocks :: Process CF ST ()
fillBlocks = do
    choked <- gets peerChoke
    interested <- gets weInterested
    when (not choked && interested) checkWatermark

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
        (do toQueue <- grabBlocks (hiMark - sz)
            when (Prelude.null toQueue) considerInterest
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
    s <- get
    let bq = blockQueue s
    q <- forM toQueue
            (\(p, b) -> do
                if S.member (p, b) bq
                    then return Nothing -- Ignore pieces which are already in queue
                    else do outChan $ SenderQ.SenderQM $ Request p b
                            return $ Just (p, b))
    put $! s { blockQueue = S.union bq (S.fromList $ catMaybes q) }

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


createAllPieces :: MonadIO m => Int -> Bool -> m PS.PieceSet
createAllPieces n False = PS.fromList n []
createAllPieces n True  = PS.fromList n [0..(n-1)]

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


-- IP address is given in host byte-order
allowedFast :: Word32 -> InfoHash -> Int -> Int -> IO [Word32]
allowedFast ip ihash sz n = generate n [] x []
  where
    -- Take pieces from the generated ones and refill when it is exhausted.
    -- While taking pieces, kill duplicates
    generate 0 pcs _ _ = return $ reverse pcs
    generate k pcs hsh (p : rest)
            | p `elem` pcs = generate k pcs hsh rest
            | otherwise    = generate (k-1) (p : pcs) hsh rest
    generate k pcs hsh [] = do
        nhsh <- Digest.digestBS hsh
        generate k pcs nhsh (genPieces nhsh)

    genPieces hash | B.null hash = []
                   | otherwise   =
                      let (h, rest) = B.splitAt 4 hash
                          bytes :: [Word32]
                          bytes    = [fromIntegral z `shiftL` s |
                                            (z, s) <- zip (B.unpack h) [24,16,8,0]]
                          ntohl = fromIntegral . sum
                      in ((ntohl bytes) `mod` fromIntegral sz) : genPieces rest
    -- To prevent a Peer to reconnect, obtain a new IP and thus new FAST-set pieces, we mask out
    -- the lower bits
    ipBytes     = B.pack $ map fromIntegral
                         [ (ip .&. 0xFF000000) `shiftR` 24
                         , (ip .&. 0x00FF0000) `shiftR` 16
                         , (ip .&. 0x0000FF00) `shiftR`  8
                         , 0
                         ]
    x = B.concat [ipBytes, ihash]

testSuite :: Test
testSuite = testGroup "Process/Peer"
    [ testCase "AllowedFast" testAllowedFast ]

testAllowedFast :: Assertion
testAllowedFast = do
    pcs <- allowedFast (ip32 [80,4,4,200]) tHash 1313 7
    assertEqual "Test1" [1059,431,808,1217,287,376,1188] pcs
    pcs' <- allowedFast (ip32 [80,4,4,200]) tHash 1313 9
    assertEqual "Test2" [1059,431,808,1217,287,376,1188,353,508] pcs'
  where ip32 :: [Int] -> Word32
        ip32 bytes = fromIntegral $ sum [b `shiftL` s | (b, s) <- zip bytes [24,16,8,0]]
        tHash :: B.ByteString
        tHash = B.pack $ take 20 (repeat 0xaa)
