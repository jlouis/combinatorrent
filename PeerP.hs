-- | Peer proceeses
module PeerP
where

import Control.Applicative hiding (empty)
import Control.Concurrent.CML
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Parser hiding (isEmpty)
import System.IO

import ConsoleP
import Queue
import Torrent
import WireProtocol

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
receiverP logCh hndl = do ch <- channel
                          spawn $ run ch
                          return ch
  where run ch =
          let lp = do l <- conv <$> B.hGet hndl 4
                      bs <- B.hGet hndl l
                      case runParser decodeMsg bs of
                        Left _ -> do sync $ transmit ch Nothing
                                     logMsg logCh "Incorrect parse in receiver, dying!"
                                     return () -- Die!
                        Right msg -> do sync $ transmit ch (Just msg)
                                        lp
          in lp
        conv :: B.ByteString -> Int
        conv bs = b4 + (256 * b3) + (256 * 256 * b2) + (256 * 256 * 256 * b1)
            where [b1,b2,b3,b4] = map fromIntegral $ B.unpack bs


data State = MkState { inCh :: Channel (Maybe Message),
                       outCh :: Channel Message,
                       peerChoke :: Bool,
                       peerInterested :: Bool,
                       peerPieces :: [PieceNum]}

peerP :: LogChannel -> Handle -> IO ()
peerP logCh h = do outBound <- sendP h
                   inBound  <- receiverP logCh h
                   spawn $ lp MkState { inCh = inBound,
                                        outCh = outBound,
                                        peerChoke = True,
                                        peerInterested = False,
                                        peerPieces = [] }
                   return ()
  where lp s = (sync $ choose [peerMsgEvent s]) >>= lp
        peerMsgEvent s = wrap (receive (inCh s) (const True))
                           (\msg ->
                                case msg of
                                  Just m -> case m of
                                              KeepAlive -> return s -- Do nothing here
                                              Choke     -> return s { peerChoke = True }
                                              Unchoke   -> return s { peerChoke = False }
                                              Interested -> return s { peerInterested = True }
                                              NotInterested -> return s { peerInterested = False }
                                              Have pn -> return  s { peerPieces = pn : (peerPieces s)}
                                              BitField bf ->
                                                  case peerPieces s of
                                                    [] -> return s { peerPieces = createPeerPieces bf }
                                                    _  -> undefined -- TODO: Kill off gracefully
                                              Request _ _ _ -> undefined
                                              Piece _ _ _ -> undefined
                                              Cancel _ _ _ -> undefined
                                              Port _ -> return s -- No DHT Yet, silently ignore
                                  Nothing -> undefined -- TODO: Kill off gracefully
                           )

createPeerPieces :: B.ByteString -> [PieceNum]
createPeerPieces _ = undefined