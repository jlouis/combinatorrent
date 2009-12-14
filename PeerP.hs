-- | Peer proceeses
module PeerP
where

import Control.Concurrent.CML
import qualified Data.ByteString.Lazy as B
import System.IO

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

data QueueState = QueueState {
      messageQueueSlow :: [Message],
      messageQueueFast :: [Message],
      sendCh :: Channel (Maybe Message) }




