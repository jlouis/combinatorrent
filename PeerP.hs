-- | Peer proceeses
module PeerP (PeerMessage(..),
              connect,
              listenHandshake,
              constructBitField)
where

import Control.Applicative hiding (empty)
import Control.Concurrent.CML
import qualified Data.ByteString.Lazy as B
import Data.Bits
import Data.ByteString.Parser hiding (isEmpty)
import Data.Maybe
import Data.Word

import Network

import System.IO


import ConsoleP
import FSP
import Queue
import Torrent
import WireProtocol

data PeerMessage = ChokePeer
                 | UnchokePeer

-- | The raw sender process, it does nothing but send out what it syncs on.
senderP :: Handle -> Channel (Maybe Message) -> IO ()
senderP sock ch = lp
  where lp = do msg <- sync $ receive ch (const True)
                case msg of
                  Nothing -> return ()
                  Just m  -> do let bs = encode m
                                B.hPut sock bs
                                lp

-- | sendQueue Process, simple version.
--   TODO: Split into fast and slow.
--   TODO: Make it possible to stop again.
sendQueueP :: Channel Message -> Channel (Maybe Message) -> IO ()
sendQueueP inC outC = lp empty
  where lp eventQ =
            do eq <- if isEmpty eventQ
                       then sync $ queueEvent eventQ
                       else sync $ choose [queueEvent eventQ, sendEvent eventQ]
               lp eq
        queueEvent q = wrap (receive inC (const True))
                        (return . push q)
        sendEvent q =
            let Just (e, r) = pop q
            in wrap (transmit outC $ Just e)
                 (const $ return r)

sendP :: Handle -> IO (Channel Message)
sendP handle = do inC <- channel
                  outC <- channel
                  spawn $ senderP handle outC
                  spawn $ sendQueueP inC outC
                  return inC


receiverP :: LogChannel -> Handle -> IO (Channel (Maybe Message))
receiverP logC hndl = do ch <- channel
                         spawn $ run ch
                         return ch
  where run ch =
          let lp = do l <- conv <$> B.hGet hndl 4
                      bs <- B.hGet hndl l
                      case runParser decodeMsg bs of
                        Left _ -> do sync $ transmit ch Nothing
                                     logMsg logC "Incorrect parse in receiver, dying!"
                                     return () -- Die!
                        Right msg -> do sync $ transmit ch (Just msg)
                                        lp
          in lp
        conv :: B.ByteString -> Int
        conv bs = b4 + (256 * b3) + (256 * 256 * b2) + (256 * 256 * 256 * b1)
            where [b1,b2,b3,b4] = map fromIntegral $ B.unpack bs


data State = MkState { inCh :: Channel (Maybe Message),
                       outCh :: Channel Message,
                       logCh :: LogChannel,
                       fsCh :: FSPChannel,
                       peerChoke :: Bool,
                       peerInterested :: Bool,
                       peerPieces :: [PieceNum]}

peerP :: FSPChannel -> LogChannel -> Handle -> IO ()
peerP fsC logC h = do
    outBound <- sendP h
    inBound  <- receiverP logC h
    spawn $ lp MkState { inCh = inBound,
                         outCh = outBound,
                         logCh = logC,
                         fsCh  = fsC,
                         peerChoke = True,
                         peerInterested = False,
                         peerPieces = [] }
    return ()
  where lp s = sync (choose [peerMsgEvent s]) >>= lp
        peerMsgEvent s = wrap (receive (inCh s) (const True))
                           (\msg ->
                                case msg of
                                  Just m -> case m of
                                              KeepAlive -> return s -- Do nothing here
                                              Choke     -> return s { peerChoke = True }
                                              Unchoke   -> return s { peerChoke = False }
                                              Interested -> return s { peerInterested = True }
                                              NotInterested -> return s { peerInterested = False }
                                              Have pn -> return  s { peerPieces = pn : peerPieces s}
                                              BitField bf ->
                                                  case peerPieces s of
                                                    [] -> return s { peerPieces = createPeerPieces bf }
                                                    _  -> undefined -- TODO: Kill off gracefully
                                              Request pn os sz ->
                                                   do c <- channel
                                                      readBlock (fsCh s) c pn os sz -- Push this down in the Send Process
                                                      bs <- sync $ receive c (const True)
                                                      sync $ transmit (outCh s) (Piece pn os bs)
                                                      return s
                                              Piece _ _ _ -> undefined
                                              Cancel _ _ _ -> undefined
                                              Port _ -> return s -- No DHT Yet, silently ignore
                                  Nothing -> undefined -- TODO: Kill off gracefully
                           )

createPeerPieces :: B.ByteString -> [PieceNum]
createPeerPieces = map fromIntegral . concat . decodeBytes 0 . B.unpack
  where decodeByte :: Int -> Word8 -> [Maybe Int]
        decodeByte soFar w =
            let dBit n = if testBit w n
                           then Just (n+soFar)
                           else Nothing
            in fmap dBit [1..8]
        decodeBytes _ [] = []
        decodeBytes soFar (w : ws) = catMaybes (decodeByte soFar w) : decodeBytes (soFar + 8) ws

constructBitField :: Integer -> [PieceNum] -> B.ByteString
constructBitField sz pieces = B.pack . build $ map (`elem` pieces) [1..sz+pad]
    where pad = 8 - (sz `mod` 8)
          build [] = []
          build l  = let (first, rest) = splitAt 8 l
                     in bytify first : build rest
          bytify bl = foldl bitSetter 0 $ zip [1..] bl
          bitSetter :: Word8 -> (Integer, Bool) -> Word8
          bitSetter w (_pos, False) = w
          bitSetter w (pos, True)  = setBit w (fromInteger pos)

connect :: HostName -> PortID -> PeerId -> InfoHash -> FSPChannel -> LogChannel
        -> IO ()
connect host port pid ih fsC logC = spawn connector >> return ()
  where connector =
         do h <- connectTo host port
            r <- initiateHandshake h pid ih
            case r of
              Left _err -> return ()
              Right (_caps, _rpid) ->
                  do peerP fsC logC h -- TODO: Pass the cpas and rpid to the peer
                     return ()

listenHandshake :: Handle -> PeerId -> InfoHash -> FSPChannel -> LogChannel
                -> IO (Either String ())
listenHandshake h pid ih fsC logC =
    do r <- initiateHandshake h pid ih
       case r of
         Left err -> return $ Left err
         Right (_caps, _rpid) -> do peerP fsC logC h -- TODO: Coerce with connect
                                    return $ Right ()
