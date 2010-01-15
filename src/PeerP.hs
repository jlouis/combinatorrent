-- | Peer proceeses
{-# LANGUAGE ScopedTypeVariables #-}
module PeerP (
    -- * Types
    PeerMessage(..),
    -- * Interface
    connect,
    unchokePeer,
    chokePeer,
    constructBitField)
where

import Control.Concurrent
import Control.Concurrent.CML
import Control.Exception
import Control.Monad

import Prelude hiding (catch)

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

-- | The call @constructBitField pieces@ will return the a ByteString suitable for inclusion in a
--   BITFIELD message to a peer.
constructBitField :: Int -> [PieceNum] -> L.ByteString
constructBitField sz pieces = L.pack . build $ m
    where m = map (`elem` pieces) [0..sz-1 + pad]
          pad = 8 - (sz `mod` 8)
          build [] = []
          build l = let (first, rest) = splitAt 8 l
                    in if length first /= 8
                       then error "Wront bitfield"
                       else bytify first : build rest
          bytify [b7,b6,b5,b4,b3,b2,b1,b0] = sum [if b0 then 1 else 0,
                                                  if b1 then 2 else 0,
                                                  if b2 then 4 else 0,
                                                  if b3 then 8 else 0,
                                                  if b4 then 16 else 0,
                                                  if b5 then 32 else 0,
                                                  if b6 then 64 else 0,
                                                  if b7 then 128 else 0]

-- INTERNAL FUNCTIONS
----------------------------------------------------------------------

-- | The raw sender process, it does nothing but send out what it syncs on.
senderP :: LogChannel -> Handle -> Channel (Maybe Message) -> SupervisorChan -> IO ThreadId
senderP logC handle ch supC = spawn startup
  where startup = lp `catches` [Handler (\ThreadKilled -> do hClose handle),
				Handler (\(ex :: IOException) -> do t <- myThreadId
								    sync $ transmit supC $ IAmDying t
								    hClose handle)]
        lp = do msg <- sync $ receive ch (const True)
                case msg of
                  Nothing -> return ()
                  Just m  -> do logMsg logC $ "Sending: " ++ show m
                                let bs = encode m
                                L.hPut handle bs
                                hFlush handle
                                logMsg logC "Sent and flushed"
                                lp

-- | Messages we can send to the Send Queue
data SendQueueMessage = SendQCancel PieceNum Block -- ^ Peer requested that we cancel a piece
                      | SendQMsg Message           -- ^ We want to send the Message to the peer
                      | SendOChoke                 -- ^ We want to choke the peer

-- | sendQueue Process, simple version.
--   TODO: Split into fast and slow.
sendQueueP :: LogChannel -> Channel SendQueueMessage -> Channel (Maybe Message) -> SupervisorChan -> IO ThreadId
sendQueueP logC inC outC supC = spawn startup
  where startup = (lp Q.empty)
	    `catches` [Handler (\ThreadKilled -> return ()),
		       Handler (\(ex :: SomeException) ->
			do logMsg logC $ "sendQueueP kill: " ++ show ex
			   mTid <- myThreadId
			   sync $ transmit supC (IAmDying mTid))]
	lp eventQ =
            do eq <- if Q.isEmpty eventQ
                       then sync $ queueEvent eventQ
                       else sync $ choose [queueEvent eventQ, sendEvent eventQ]
               lp eq
        queueEvent q = wrap (receive inC (const True))
                        (\m -> case m of
                                 SendQMsg msg -> do logMsg logC "Queueing event for sending"
                                                    return $ Q.push msg q
                                 SendQCancel n blk -> return $ Q.filter (filterPiece n (blockOffset blk)) q
                                 SendOChoke -> do let nq = Q.filter filterAllPiece q
                                                  return $ Q.push Choke nq)
        filterAllPiece (Piece _ _ _) = True
        filterAllPiece _             = False
        filterPiece n off m =
	    case m of Piece n off _ -> False
                      _             -> True
        sendEvent q =
            let Just (e, r) = Q.pop q
            in wrap (transmit outC $ Just e)
                   (\() -> do logMsg logC "Sent event"
                              return r)

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

receiverP :: LogChannel -> Handle -> Channel (Maybe Message) -> SupervisorChan -> IO ThreadId
receiverP logC hndl ch supC = spawn startup
  where startup = (lp ch) `catches` [Handler (\ThreadKilled -> return ()),
				     Handler (\(ex :: IOException) -> do mTid <- myThreadId
									 sync $ transmit supC $ IAmDying mTid)]
	lp ch = do logMsg logC "Peer waiting for input"
                   readHeader ch
        -- This checking could be done when we read 0 bytes and conversion fails.
        readHeader ch = do
          feof <- hIsEOF hndl
          if feof
            then do logMsg logC "Handle is closed, dying!"
                    return ()
            else do bs' <- L.hGet hndl 4
                    l <- conv bs'
                    readMessage l ch
        readMessage l ch = do
          when (l == 0) (lp ch)
          logMsg logC $ "Reading off " ++ show l ++ " bytes"
          bs <- L.hGet hndl (fromIntegral l)
          case runParser decodeMsg bs of
            Left _ -> do sync $ transmit ch Nothing
                         logMsg logC "Incorrect parse in receiver, dying!"
                         return () -- Die!
            Right msg -> do sync $ transmit ch (Just msg)
                            lp ch
        conv :: L.ByteString -> IO Word32
        conv bs = do
          logMsg logC $ show $ L.length bs
          case runParser getWord32be bs of
                    Left err -> do logMsg logC $ "Incorrent parse in receiver, dying: " ++ show err
                                   error "receiverP: Incorrent length in receiver, dying!"
                    Right i -> return i

data State = MkState { inCh :: Channel (Maybe Message),
                       outCh :: Channel SendQueueMessage,
                       pieceMgrCh :: PieceMgrChannel,
                       logCh :: LogChannel,
                       fsCh :: FSPChannel,
                       peerCh :: PeerChannel,
                       weChoke :: Bool,
                       pieceMap :: PieceMap,
                       blockQueue :: S.Set (PieceNum, Block),
                       peerChoke :: Bool,
                       peerInterested :: Bool,
                       peerPieces :: [PieceNum]}

-- TODO: Consider filling blocks after each loop...
peerP :: MgrChannel -> PieceMgrChannel -> FSPChannel -> PieceMap -> LogChannel -> Int -> Handle
         -> Channel SendQueueMessage -> Channel (Maybe Message) 
	 -> SupervisorChan -> IO ThreadId
peerP pMgrC pieceMgrC fsC pm logC nPieces h outBound inBound supC = do
    logMsg logC "Spawning Peer process"
    t <- spawn $ do
      tid <- myThreadId
      logMsg logC "Syncing a connect Back"
      ch <- channel
      -- The Connect must happen before the bitfield is transferred. That way any piece getting done
      -- in the meantime will be sent as one of the first "normal" messages to the peer.
      sync $ transmit pMgrC $ Connect tid ch
      pieces <- PieceMgrP.getPieceDone pieceMgrC
      sync $ transmit outBound $ SendQMsg $ BitField (constructBitField nPieces pieces)
      sync $ transmit outBound $ SendQMsg Interested
      (lp MkState { inCh = inBound,
		    outCh = outBound,
                    logCh = logC,
                    peerCh = ch,
                    fsCh  = fsC,
                    pieceMgrCh = pieceMgrC,
                    pieceMap = pm,
                    blockQueue = S.empty,
                    weChoke = True,
                    peerChoke = True,
                    peerInterested = False,
                    peerPieces = [] }) `catches`
	    [Handler (\ThreadKilled -> do
	        stop pMgrC),
	     Handler (\(ex :: SomeException) -> do
		logMsg logC $ "PeerP Killed: " ++ show ex
		stop pMgrC
		mTid <- myThreadId
		sync $ transmit supC $ IAmDying mTid)]
    return t
  where lp s = sync (choose [peerMsgEvent s, chokeMgrEvent s]) >>= lp
	stop pMgrC = do mtid <- myThreadId
			disconnectPeer pMgrC mtid
        chokeMgrEvent s = wrap (receive (peerCh s) (const True))
                           (\msg ->
                                case msg of
                                     ChokePeer -> do sync $ transmit (outCh s) SendOChoke
                                                     return s { weChoke = True }
                                     UnchokePeer -> do sync $ transmit (outCh s) $ SendQMsg Unchoke
                                                       return s { weChoke = False }
			             PeerStats retCh -> do sync $ transmit retCh (0.0, -- TODO: Fix
										  peerInterested s)
							   return s)
        peerMsgEvent s = wrap (receive (inCh s) (const True))
                           (\msg ->
                                case msg of
                                  Just m -> case m of
                                              KeepAlive -> return s -- Do nothing here
                                              Choke     -> do PieceMgrP.putbackBlocks
                                                                           (pieceMgrCh s)
                                                                           (S.toList $ blockQueue s)
                                                              return s { blockQueue = S.empty }
                                              Unchoke   -> fillBlocks s { peerChoke = False }
                                              -- The next two is dependent in the PeerManager being more clever
                                              Interested -> return s { peerInterested = True } -- TODO
                                              NotInterested -> return s { peerInterested = False } -- TODO
                                              Have pn ->
                                                  if M.member pn (pieceMap s)
                                                     then fillBlocks s { peerPieces = pn : peerPieces s }
                                                     else error "Unknown piece" -- TODO: Handle error properly
                                              BitField bf ->
                                                  case peerPieces s of
                                                    -- TODO: Don't trust the BitField
                                                    [] -> fillBlocks s { peerPieces = createPeerPieces bf }
                                                    _  -> error "Out of band BitField request" -- TODO: Kill off gracefully
                                              Request pn blk ->
                                                  if weChoke s
                                                    then return s -- Ignore, there might be stray packets
                                                    else
                                                        do c <- channel
                                                           readBlock (fsCh s) c pn blk -- TODO: Pushdown in Send Process
                                                           bs <- sync $ receive c (const True)
                                                           sync $ transmit (outCh s) $
                                                                SendQMsg (Piece pn (blockOffset blk) bs)
                                                           return s
                                              Piece n os bs ->
                                                  let sz = B.length bs
                                                      blk = Block os sz
                                                      e = (n, blk)
                                                  in if S.member e (blockQueue s)
                                                       then do PieceMgrP.storeBlock (pieceMgrCh s) n (Block os sz) bs
                                                               fillBlocks s { blockQueue = S.delete e (blockQueue s) }
                                                       else fillBlocks s -- Piece might be stray
                                              Cancel n blk -> do sync $ transmit (outCh s) $ SendQCancel n blk
                                                                 return s
                                              Port _ -> return s -- No DHT Yet, silently ignore
                                  Nothing -> do logMsg (logCh s) "Unknown message"
                                                undefined -- TODO: Kill off gracefully
                           )
        fillBlocks s = if peerChoke s
                         then return s
                         else checkWatermark s
        checkWatermark s =
            let sz = S.size (blockQueue s)
            in if sz < loMark
                 then do
                   logMsg logC $ "Filling with " ++ show (hiMark - sz) ++ " pieces..."
                   toQueue <- PieceMgrP.grabBlocks (pieceMgrCh s) (hiMark - sz) (peerPieces s)
                   logMsg logC $ "Got " ++ show (length toQueue) ++ " blocks"
                   queuePieces s toQueue
                 else return s -- Do nothing, we have plenty queued already
        queuePieces s toQueue = do mapM_ (pushPiece $ outCh s) toQueue
                                   return s { blockQueue = S.union (blockQueue s) (S.fromList toQueue) }
        pushPiece ch (pn, blk) =
            sync $ transmit ch $ SendQMsg $ Request pn blk
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
