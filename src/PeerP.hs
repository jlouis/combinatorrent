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
import Data.Word

import Network

import System.IO

import PeerTypes
import Process
import Logging
import FSP hiding (pieceMap)
import PieceMgrP
import qualified Queue as Q
import Supervisor
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
        -> MgrChannel -> Int
        -> IO ThreadId
connect (host, port, pid, ih, pm) pool pieceMgrC fsC logC mgrC nPieces = spawn (connector >> return ())
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
		     children <- peerChildren logC h mgrC pieceMgrC fsC pm nPieces
		     sync $ transmit pool $ SpawnNew (Supervisor $ allForOne children)
		     return ()

-- INTERNAL FUNCTIONS
----------------------------------------------------------------------

data SPCF = SPCF { spLogCh :: LogChannel
		 , spMsgCh :: Channel Message
		 }

instance Logging SPCF where
   getLogger = spLogCh

-- | The raw sender process, it does nothing but send out what it syncs on.
senderP :: LogChannel -> Handle -> Channel Message -> SupervisorChan -> IO ThreadId
senderP logC h ch supC = spawnP (SPCF logC ch) h (catchP (foreverP pgm)
						    (do t <- liftIO $ myThreadId
							syncP =<< (sendP supC $ IAmDying t)
							liftIO $ hClose h))
  where
    pgm :: Process SPCF Handle ()
    pgm = do
	c <- ask
	m <- syncP =<< recvPC spMsgCh
	let bs = encode m
	h <- get
	liftIO $ do L.hPut h bs
	            hFlush h
	Process.log "Sent and flushed msg"

-- | Messages we can send to the Send Queue
data SendQueueMessage = SendQCancel PieceNum Block -- ^ Peer requested that we cancel a piece
                      | SendQMsg Message           -- ^ We want to send the Message to the peer
                      | SendOChoke                 -- ^ We want to choke the peer

data SQCF = SQCF { sqLogC :: LogChannel
		 , sqInCh :: Channel SendQueueMessage
		 , sqOutCh :: Channel Message
		 }

instance Logging SQCF where
  getLogger = sqLogC

-- | sendQueue Process, simple version.
--   TODO: Split into fast and slow.
sendQueueP :: LogChannel -> Channel SendQueueMessage -> Channel Message -> SupervisorChan -> IO ThreadId
sendQueueP logC inC outC supC = spawnP (SQCF logC inC outC) Q.empty (catchP (foreverP pgm)
								        (defaultStopHandler supC))
  where
    pgm = do
	q <- get
	if Q.isEmpty q
	    then queueEvent >>= syncP
	    else chooseP [queueEvent, sendEvent] >>= syncP
    queueEvent = do
	ev <- recvPC sqInCh
	wrapP ev (\m -> case m of
			SendQMsg msg -> do log "Queueing event for sending"
					   modify (Q.push msg)
			SendQCancel n blk -> modify (Q.filter (filterPiece n (blockOffset blk)))
			SendOChoke -> do modify (Q.filter filterAllPiece)
					 modify (Q.push Choke))
    sendEvent = do
	Just (e, r) <- gets Q.pop
	tEvt <- sendPC sqOutCh e
	wrapP tEvt (\() -> do log "Dequeued event"
			      put r)
    filterAllPiece (Piece _ _ _) = True
    filterAllPiece _             = False
    filterPiece n off m =
        case m of Piece n off _ -> False
                  _             -> True


peerChildren :: LogChannel -> Handle -> MgrChannel -> PieceMgrChannel
	     -> FSPChannel -> PieceMap -> Int -> IO Children
peerChildren logC handle pMgrC pieceMgrC fsC pm nPieces = do
    queueC <- channel
    senderC <- channel
    receiverC <- channel
    return [Worker $ senderP logC handle senderC,
	    Worker $ sendQueueP logC queueC senderC,
	    Worker $ receiverP logC handle receiverC,
	    Worker $ peerP pMgrC pieceMgrC fsC pm logC nPieces handle queueC receiverC]

data RPCF = RPCF { rpLogC :: LogChannel
                 , rpMsgC :: Channel (Message, Integer) }

instance Logging RPCF where
  getLogger = rpLogC

receiverP :: LogChannel -> Handle -> Channel (Message, Integer) -> SupervisorChan -> IO ThreadId
receiverP logC h ch supC = spawnP (RPCF logC ch) h
	(catchP (foreverP pgm)
	       (defaultStopHandler supC))
  where
    pgm = do log "Peer waiting for input"
             readHeader ch
    readHeader ch = do
        h <- get
	feof <- liftIO $ hIsEOF h
	if feof
	    then do log "Handle Closed"
		    stopP
	    else do bs' <- liftIO $ L.hGet h 4
		    l <- conv bs'
		    readMessage l ch
    readMessage l ch = do
        if (l == 0)
	    then return ()
	    else do log $ "Reading off " ++ show l ++ " bytes"
		    h <- get
		    bs <- liftIO $ L.hGet h (fromIntegral l)
		    case runParser decodeMsg bs of
			Left _ -> do log "Incorrect parse in receiver, dying!"
                                     stopP
                        Right msg -> do sendPC rpMsgC (msg, fromIntegral l) >>= syncP
    conv bs = do
        log $ show $ L.length bs
        case runParser getWord32be bs of
          Left err -> do log $ "Incorrent parse in receiver, dying: " ++ show err
                         stopP
          Right i -> return i

data PCF = PCF { inCh :: Channel (Message, Integer)
	       , outCh :: Channel SendQueueMessage
	       , peerMgrCh :: MgrChannel
	       , pieceMgrCh :: PieceMgrChannel
	       , logCh :: LogChannel
	       , fsCh :: FSPChannel
	       , peerCh :: PeerChannel
	       , pieceMap :: PieceMap
	       }

instance Logging PCF where
  getLogger = logCh

data PST = PST { weChoke :: Bool
	       , weInterested :: Bool
	       , blockQueue :: S.Set (PieceNum, Block)
	       , peerChoke :: Bool
	       , peerInterested :: Bool
	       , peerPieces :: [PieceNum]
	       }

peerP :: MgrChannel -> PieceMgrChannel -> FSPChannel -> PieceMap -> LogChannel -> Int -> Handle
         -> Channel SendQueueMessage -> Channel (Message, Integer)
	 -> SupervisorChan -> IO ThreadId
peerP pMgrC pieceMgrC fsC pm logC nPieces h outBound inBound supC = do
    ch <- channel
    spawnP (PCF inBound outBound pMgrC pieceMgrC logC fsC ch pm)
	   (PST True False S.empty True False [])
	   (cleanupP startup (defaultStopHandler supC) cleanup)
  where startup = do
	    tid <- liftIO $ myThreadId
	    log "Syncing a connectBack"
	    asks peerCh >>= (\ch -> sendPC peerMgrCh $ Connect tid ch) >>= syncP
	    pieces <- getPiecesDone
	    syncP =<< (sendPC outCh $ SendQMsg $ BitField (constructBitField nPieces pieces))
	    foreverP (recvEvt >> fillBlocks)
	cleanup = do
	    t <- liftIO myThreadId
	    syncP =<< sendPC peerMgrCh (Disconnect t)
        getPiecesDone = do
	    c <- liftIO $ channel
	    syncP =<< (sendPC pieceMgrCh $ GetDone c)
	    recvP c (const True) >>= syncP
	recvEvt = do
	    syncP =<< chooseP [peerMsgEvent, chokeMgrEvent]
	chokeMgrEvent = do
	    evt <- recvPC peerCh
	    wrapP evt (\msg ->
		case msg of
		    PieceCompleted pn -> do
			syncP =<< (sendPC outCh $ SendQMsg $ Have pn)
		    ChokePeer -> do syncP =<< sendPC outCh SendOChoke
				    modify (\s -> s {weChoke = True})
		    UnchokePeer -> do syncP =<< (sendPC outCh $ SendQMsg Unchoke)
				      modify (\s -> s {weChoke = False})
		    PeerStats retCh -> do i <- gets peerInterested
					  syncP =<< sendP retCh (0.0, i)) -- TODO: Fix
	peerMsgEvent = do
	    evt <- recvPC inCh
	    wrapP evt (\(msg, _) ->
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
		else do log "Unknown Piece"
		        stopP
	bitfieldMsg bf = do
	    pieces <- gets peerPieces
	    case pieces of
	      -- TODO: Don't trust the bitfield
	      [] -> do modify (\s -> s { peerPieces = createPeerPieces bf})
		       considerInterest
	      _  -> do log "Got out of band Bitfield request, dying"
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
                   log $ "Filling with " ++ show (hiMark - sz) ++ " pieces..."
		   toQueue <- grabBlocks (hiMark - sz)
                   log $ "Got " ++ show (length toQueue) ++ " blocks"
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
	    return [(pn, b) | (pn, blklst) <- blks, b <- blklst]
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
