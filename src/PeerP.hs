-- | Peer proceeses
{-# LANGUAGE ScopedTypeVariables #-}
module PeerP (
    -- * Types
      PeerMessage(..)
    -- * Interface
    , connect
    , unchokePeer
    , chokePeer
    )
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.CML
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Prelude hiding (catch, log)

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Parser hiding (isEmpty)
import qualified Data.Map as M
import Data.List (sort)
import Data.Maybe

import Data.Set as S hiding (map)
import Data.Time.Clock
import Data.Word

import Network

import System.IO

import PeerTypes
import Process
import Logging
import FSP
import PieceMgrP
import qualified Queue as Q
import RateCalc as RC
import StatusP
import Supervisor
import TimerP
import Torrent
import WireProtocol

-- INTERFACE
----------------------------------------------------------------------

-- | Send a choke message to the peer process at PeerChannel. May raise
--   exceptions if the peer is not running anymore.
chokePeer :: PeerChannel -> IO ()
chokePeer ch = sync $ transmit ch ChokePeer

-- | Send an unchoke message to the peer process at PeerChannel. May raise
--   exceptions if the peer is not running anymore.
unchokePeer :: PeerChannel -> IO ()
unchokePeer ch = sync $ transmit ch UnchokePeer

type ConnectRecord = (HostName, PortID, PeerId, InfoHash, PieceMap)

connect :: ConnectRecord -> SupervisorChan -> PieceMgrChannel -> FSPChannel -> LogChannel
	-> StatusChan
        -> MgrChannel -> Int
        -> IO ThreadId
connect (host, port, pid, ih, pm) pool pieceMgrC fsC logC statC mgrC nPieces =
    spawn (connector >> return ())
  where connector =
         do logMsg logC $ "Connecting to " ++ show host ++ " (" ++ showPort port ++ ")"
            h <- connectTo host port
            logMsg logC "Connected, initiating handShake"
            r <- initiateHandshake logC h pid ih
            logMsg logC "Handshake run"
            case r of
              Left err -> do logMsg logC ("Peer handshake failure at host " ++ host
                                              ++ " with error " ++ err)
                             return ()
              Right (_caps, _rpid) ->
                  do logMsg logC "entering peerP loop code"
		     supC <- channel -- TODO: Should be linked later on
		     children <- peerChildren logC h mgrC pieceMgrC fsC statC pm nPieces
		     sync $ transmit pool $ SpawnNew (Supervisor $ allForOne "PeerSup" children logC)
		     return ()

-- INTERNAL FUNCTIONS
----------------------------------------------------------------------

data SPCF = SPCF { spLogCh :: LogChannel
		 , spMsgCh :: Channel L.ByteString
		 }

instance Logging SPCF where
   getLogger cf = ("SenderP", spLogCh cf)

-- | The raw sender process, it does nothing but send out what it syncs on.
senderP :: LogChannel -> Handle -> Channel L.ByteString -> SupervisorChan -> IO ThreadId
senderP logC h ch supC = spawnP (SPCF logC ch) h (catchP (foreverP pgm)
						    (do t <- liftIO $ myThreadId
							syncP =<< (sendP supC $ IAmDying t)
							liftIO $ hClose h))
  where
    pgm :: Process SPCF Handle ()
    pgm = do
	c <- ask
	m <- syncP =<< recvPC spMsgCh
	h <- get
	liftIO $ do L.hPut h m
	            hFlush h

-- | Messages we can send to the Send Queue
data SendQueueMessage = SendQCancel PieceNum Block -- ^ Peer requested that we cancel a piece
                      | SendQMsg Message           -- ^ We want to send the Message to the peer
                      | SendOChoke                 -- ^ We want to choke the peer
		      | SendQRequestPrune PieceNum Block -- ^ Prune SendQueue of this (pn, blk) pair

data SQCF = SQCF { sqLogC :: LogChannel
		 , sqInCh :: Channel SendQueueMessage
		 , sqOutCh :: Channel L.ByteString
		 , bandwidthCh :: BandwidthChannel
		 }

data SQST = SQST { outQueue :: Q.Queue Message
		 , bytesTransferred :: Integer
		 }

instance Logging SQCF where
  getLogger cf = ("SendQueueP", sqLogC cf)

-- | sendQueue Process, simple version.
--   TODO: Split into fast and slow.
sendQueueP :: LogChannel -> Channel SendQueueMessage -> Channel L.ByteString -> BandwidthChannel 
	   -> SupervisorChan
	   -> IO ThreadId
sendQueueP logC inC outC bandwC supC = spawnP (SQCF logC inC outC bandwC) (SQST Q.empty 0)
	(catchP (foreverP pgm)
	        (defaultStopHandler supC))
  where
    pgm :: Process SQCF SQST ()
    pgm = do
	q <- gets outQueue
	l <- gets bytesTransferred
	case (Q.isEmpty q, l > 0) of
	    (True, False) -> queueEvent >>= syncP
	    (True, True ) -> chooseP [queueEvent, rateUpdateEvent] >>= syncP
	    (False, False) -> chooseP [queueEvent, sendEvent] >>= syncP
	    (False, True)  -> chooseP [queueEvent, sendEvent, rateUpdateEvent] >>= syncP
    rateUpdateEvent = do
	l <- gets bytesTransferred
	ev <- sendPC bandwidthCh l
	wrapP ev (\() ->
	    modify (\s -> s { bytesTransferred = 0 }))
    queueEvent = do
	ev <- recvPC sqInCh
	wrapP ev (\m -> case m of
			SendQMsg msg -> do logDebug "Queueing event for sending"
					   modifyQ (Q.push msg)
			SendQCancel n blk -> modifyQ (Q.filter (filterPiece n (blockOffset blk)))
			SendOChoke -> do modifyQ (Q.filter filterAllPiece)
					 modifyQ (Q.push Choke)
			SendQRequestPrune n blk ->
			    modifyQ (Q.filter (filterRequest n blk)))
    modifyQ :: (Q.Queue Message -> Q.Queue Message) -> Process SQCF SQST ()
    modifyQ f = modify (\s -> s { outQueue = f (outQueue s) })
    sendEvent = do
	Just (e, r) <- gets (Q.pop . outQueue)
	let bs = encode e
	tEvt <- sendPC sqOutCh bs
	wrapP tEvt (\() -> do logDebug "Dequeued event"
			      modify (\s -> s { outQueue = r,
					        bytesTransferred =
						    bytesTransferred s + fromIntegral (L.length bs)}))
    filterAllPiece (Piece _ _ _) = True
    filterAllPiece _             = False
    filterPiece n off m =
        case m of Piece n off _ -> False
                  _             -> True
    filterRequest n blk m =
	case m of Request n blk -> False
	          _             -> True

peerChildren :: LogChannel -> Handle -> MgrChannel -> PieceMgrChannel
	     -> FSPChannel -> StatusChan -> PieceMap -> Int -> IO Children
peerChildren logC handle pMgrC pieceMgrC fsC statusC pm nPieces = do
    queueC <- channel
    senderC <- channel
    receiverC <- channel
    sendBWC <- channel
    return [Worker $ senderP logC handle senderC,
	    Worker $ sendQueueP logC queueC senderC sendBWC,
	    Worker $ receiverP logC handle receiverC,
	    Worker $ peerP pMgrC pieceMgrC fsC pm logC nPieces handle
				queueC receiverC sendBWC statusC]

data RPCF = RPCF { rpLogC :: LogChannel
                 , rpMsgC :: Channel (Message, Integer) }

instance Logging RPCF where
  getLogger cf = ("ReceiverP", rpLogC cf)

receiverP :: LogChannel -> Handle -> Channel (Message, Integer) -> SupervisorChan -> IO ThreadId
receiverP logC h ch supC = spawnP (RPCF logC ch) h
	(catchP (foreverP pgm)
	       (defaultStopHandler supC))
  where
    pgm = do logDebug "Peer waiting for input"
             readHeader ch
    readHeader ch = do
        h <- get
	feof <- liftIO $ hIsEOF h
	if feof
	    then do logDebug "Handle Closed"
		    stopP
	    else do bs' <- liftIO $ L.hGet h 4
		    l <- conv bs'
		    readMessage l ch
    readMessage l ch = do
        if (l == 0)
	    then return ()
	    else do logDebug $ "Reading off " ++ show l ++ " bytes"
		    h <- get
		    bs <- liftIO $ L.hGet h (fromIntegral l)
		    case runParser decodeMsg bs of
			Left _ -> do logWarn "Incorrect parse in receiver, dying!"
                                     stopP
                        Right msg -> do sendPC rpMsgC (msg, fromIntegral l) >>= syncP
    conv bs = do
        case runParser getWord32be bs of
          Left err -> do logWarn $ "Incorrent parse in receiver, dying: " ++ show err
                         stopP
          Right i -> return i

data PCF = PCF { inCh :: Channel (Message, Integer)
	       , outCh :: Channel SendQueueMessage
	       , peerMgrCh :: MgrChannel
	       , pieceMgrCh :: PieceMgrChannel
	       , logCh :: LogChannel
	       , fsCh :: FSPChannel
	       , peerCh :: PeerChannel
	       , sendBWCh :: BandwidthChannel
	       , timerCh :: Channel ()
	       , statCh :: StatusChan
	       , pieceMap :: PieceMap
	       }

instance Logging PCF where
  getLogger cf = ("PeerP", logCh cf)

data PST = PST { weChoke :: Bool -- ^ True if we are choking the peer
	       , weInterested :: Bool -- ^ True if we are interested in the peer
	       , blockQueue :: S.Set (PieceNum, Block) -- ^ Blocks queued at the peer
	       , peerChoke :: Bool -- ^ Is the peer choking us? True if yes
	       , peerInterested :: Bool -- ^ True if the peer is interested
	       , peerPieces :: [PieceNum] -- ^ List of pieces the peer has access to
	       , upRate :: Rate -- ^ Upload rate towards the peer (estimated)
	       , downRate :: Rate -- ^ Download rate from the peer (estimated)
	       , runningEndgame :: Bool -- ^ True if we are in endgame
	       }

peerP :: MgrChannel -> PieceMgrChannel -> FSPChannel -> PieceMap -> LogChannel -> Int -> Handle
         -> Channel SendQueueMessage -> Channel (Message, Integer) -> BandwidthChannel
	 -> StatusChan
	 -> SupervisorChan -> IO ThreadId
peerP pMgrC pieceMgrC fsC pm logC nPieces h outBound inBound sendBWC statC supC = do
    ch <- channel
    tch <- channel
    ct <- getCurrentTime
    spawnP (PCF inBound outBound pMgrC pieceMgrC logC fsC ch sendBWC tch statC pm)
	   (PST True False S.empty True False [] (RC.new ct) (RC.new ct) False)
	   (cleanupP startup (defaultStopHandler supC) cleanup)
  where startup = do
	    tid <- liftIO $ myThreadId
	    logDebug "Syncing a connectBack"
	    asks peerCh >>= (\ch -> sendPC peerMgrCh $ Connect tid ch) >>= syncP
	    pieces <- getPiecesDone
	    syncP =<< (sendPC outCh $ SendQMsg $ BitField (constructBitField nPieces pieces))
	    -- Install the StatusP timer
	    c <- asks timerCh
	    TimerP.register 30 () c
	    foreverP (recvEvt >> fillBlocks)
	cleanup = do
	    t <- liftIO myThreadId
	    syncP =<< sendPC peerMgrCh (Disconnect t)
        getPiecesDone = do
	    c <- liftIO $ channel
	    syncP =<< (sendPC pieceMgrCh $ GetDone c)
	    recvP c (const True) >>= syncP
	recvEvt = do
	    syncP =<< chooseP [peerMsgEvent, chokeMgrEvent, upRateEvent, timerEvent]
	chokeMgrEvent = do
	    evt <- recvPC peerCh
	    wrapP evt (\msg -> do
		logDebug "ChokeMgrEvent"
		case msg of
		    PieceCompleted pn -> do
			syncP =<< (sendPC outCh $ SendQMsg $ Have pn)
		    ChokePeer -> do syncP =<< sendPC outCh SendOChoke
				    logDebug "Choke Peer"
				    modify (\s -> s {weChoke = True})
		    UnchokePeer -> do syncP =<< (sendPC outCh $ SendQMsg Unchoke)
				      logDebug "UnchokePeer"
				      modify (\s -> s {weChoke = False})
		    PeerStats t retCh -> do
			i <- gets peerInterested
			ur <- gets upRate
			dr <- gets downRate
			let (up, nur) = RC.extractRate t ur
			    (down, ndr) = RC.extractRate t dr
			logInfo $ "Peer has rates up/down: " ++ show up ++ "/" ++ show down
			sendP retCh (up, down, i) >>= syncP
			modify (\s -> s { upRate = nur , downRate = ndr })
		    CancelBlock pn blk -> do
			modify (\s -> s { blockQueue = S.delete (pn, blk) $ blockQueue s })
			syncP =<< (sendPC outCh $ SendQRequestPrune pn blk))
	timerEvent = do
	    evt <- recvPC timerCh
	    wrapP evt (\() -> do
		logDebug "TimerEvent"
	        tch <- asks timerCh
		TimerP.register 30 () tch
		ur <- gets upRate
		dr <- gets downRate
		let (upCnt, nuRate) = RC.extractCount $ ur
		    (downCnt, ndRate) = RC.extractCount $ dr
		(sendPC statCh $ PeerStat { peerUploaded = upCnt
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
		  Unchoke    -> modify (\s -> s { peerChoke = False })
                  -- The next two is dependent in the PeerManager being more clever
                  Interested -> modify (\s -> s { peerInterested = True })
		  NotInterested -> modify (\s -> s { peerInterested = False })
		  Have pn -> haveMsg pn
		  BitField bf -> bitfieldMsg bf
		  Request pn blk -> requestMsg pn blk
		  Piece n os bs -> pieceMsg n os bs
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
		then do modify (\s -> s { peerPieces = pn : peerPieces s})
		        considerInterest
		else do logWarn "Unknown Piece"
		        stopP
	bitfieldMsg bf = do
	    pieces <- gets peerPieces
	    case pieces of
	      -- TODO: Don't trust the bitfield
	      [] -> do modify (\s -> s { peerPieces = createPeerPieces bf})
		       considerInterest
	      _  -> do logInfo "Got out of band Bitfield request, dying"
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
	    -- When e is not a member, the piece may be stray, so ignore it. Perhaps print something
	    --   here.
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
	    choking <- gets peerChoke
	    unless choking checkWatermark
        checkWatermark = do
	    q <- gets blockQueue
	    let sz = S.size q
	    when (sz < loMark)
		(do
                   logDebug $ "Filling with " ++ show (hiMark - sz) ++ " pieces..."
		   toQueue <- grabBlocks (hiMark - sz)
                   logDebug $ "Got " ++ show (length toQueue) ++ " blocks"
                   queuePieces toQueue)
	queuePieces toQueue = do
	    mapM_ pushPiece toQueue
	    modify (\s -> s { blockQueue = S.union (blockQueue s) (S.fromList toQueue) })
	pushPiece (pn, blk) =
	    syncP =<< sendPC outCh (SendQMsg $ Request pn blk)
	storeBlock n blk bs =
	    syncP =<< sendPC fsCh (WriteBlock n blk bs)
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
        hiMark = 15 -- These two values are chosen rather arbitrarily at the moment.

createPeerPieces :: L.ByteString -> [PieceNum]
createPeerPieces = map fromIntegral . concat . decodeBytes 0 . L.unpack
  where decodeByte :: Int -> Word8 -> [Maybe Int]
        decodeByte soFar w =
            let dBit n = if testBit w (7-n)
                           then Just (n+soFar)
                           else Nothing
            in fmap dBit [0..7]
        decodeBytes _ [] = []
        decodeBytes soFar (w : ws) = catMaybes (decodeByte soFar w) : decodeBytes (soFar + 8) ws


showPort :: PortID -> String
showPort (PortNumber pn) = show pn
showPort _               = "N/A"

disconnectPeer :: MgrChannel -> ThreadId -> IO ()
disconnectPeer c t = sync $ transmit c $ Disconnect t


-- TODO: Consider if this code is correct with what we did to [connect]
{-
listenHandshake :: Handle -> PeerId -> InfoHash -> FSPChannel -> LogChannel
                -> MgrChannel
                -> IO (Either String ())
listenHandshake h pid ih fsC logC mgrC =
    do r <- initiateHandshake logC h pid ih
       case r of
         Left err -> return $ Left err
         Right (_caps, _rpid) -> do peerP mgrC fsC logC h -- TODO: Coerce with connect
                                    return $ Right ()
-}
