-- | Peer proceeses
module PeerP
where

import Control.Concurrent.CML
import qualified Data.ByteString.Lazy as B
import System.IO

import WireProtocol
import Queue

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
                         (\msg -> return $ push q msg)
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
