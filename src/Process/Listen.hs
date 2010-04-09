module Process.Listen
    ( start
    )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader

import Network

import Process
import Process.PeerMgr hiding (start)
import Supervisor

data CF = CF { peerMgrCh :: PeerMgrChannel }

instance Logging CF where
    logName _ = "Process.Listen"

start :: PortID -> PeerMgrChannel -> SupervisorChannel -> IO ThreadId
start port peerMgrC supC = do
    spawnP (CF peerMgrC) () (catchP (openListen >>= pgm)
                        (defaultStopHandler supC)) -- TODO: Close socket resource!
  where openListen = liftIO $ listenOn port
        pgm sock = forever lp
          where lp = do c <- asks peerMgrCh
                        liftIO $ do
                           conn <- accept sock
                           atomically $ writeTChan c (NewIncoming conn)

