module Process.Peer.Receiver
    ( start )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (assert)

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Prelude hiding (catch, log)

import qualified Data.Attoparsec as A

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy

import Channels
import Process
import Supervisor
import Protocol.Wire


data CF = CF { rpMsgCh :: TChan MsgTy }

instance Logging CF where
    logName _ = "Process.Peer.Receiver"

demandInput :: Int -> Process CF Socket L.ByteString
demandInput l = {-# SCC "demandInput" #-} do
    s <- get
    bs <- liftIO $ recv s (fromIntegral l)
    when (L.null bs) stopP
    return bs

start :: Socket -> TChan MsgTy -> SupervisorChannel -> IO ThreadId
start s ch supC = do
   spawnP (CF ch) s
        ({-# SCC "Receiver" #-} catchP readSend
               (defaultStopHandler supC))

readSend :: Process CF Socket ()
readSend = do
    bs <- demandInput 2048
    loopHeader bs


loopHeader :: L.ByteString -> Process CF Socket ()
loopHeader bs = {-# SCC "loopHeader" #-}
    let bsl = L.length bs
    in if bsl >= 4
         then let (l, r) = L.splitAt 4 bs
                  ll = readW32 l
              in if ll == 0
                    then loopHeader r -- KeepAlive
                    else loopMsg [r] (fromIntegral (L.length r)) (readW32 l)
         else do
            inp <- demandInput 2048
            loopHeader (L.concat [bs, inp]) -- We bet on this get called rarely

loopMsg :: [L.ByteString] -> Int -> Int -> Process CF Socket ()
loopMsg lbs sz l = {-# SCC "loopMsg" #-} do
    if sz >= l
        then do let (u, r) =
                     L.splitAt (fromIntegral l)
                                (case lbs of
                                    [x] -> x
                                    rest -> (L.concat $ reverse rest))
                msg <- assert (L.length u == fromIntegral l) parseMsg l u
                c <- asks rpMsgCh
                liftIO . atomically $ writeTChan c (FromPeer (msg, fromIntegral l))
                loopHeader r
        else do inp <- demandInput (l - sz)
                loopMsg (inp : lbs) (sz + fromIntegral (L.length inp)) l

readW32 :: L.ByteString -> Int
readW32 lbs = {-# SCC "readW32" #-}
    let [b1,b2,b3,b4] = L.unpack lbs
        b1' = fromIntegral b1
        b2' = fromIntegral b2
        b3' = fromIntegral b3
        b4' = fromIntegral b4
    in (b4' + (256 * b3') + (256 * 256 * b2') + (256 * 256 * 256 * b1'))

parseMsg :: Int -> L.ByteString -> Process CF Socket Message
parseMsg l u = {-# SCC "parseMsg" #-}
    case A.parse (getAPMsg l) (B.concat $ L.toChunks u) of
        A.Done r msg -> assert (B.null r) (return msg)
        A.Fail _ ctx err ->
            do warningP $ "Incorrect parse in receiver, context: "
                                ++ show ctx ++ ", " ++ show err
               stopP
        A.Partial _k ->
            do errorP "Can't happen, impossible"
               stopP

