module Process.Listen
    ( start
    )
where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans

import Network

import Process
import Process.PeerMgr hiding (start)
import Supervisor

data CF = CF { peerMgrCh :: PeerMgrChannel
             }

instance Logging CF where
    logName _ = "Process.Listen"

start :: PortID -> PeerMgrChannel -> SupervisorChan -> IO ThreadId
start port peerMgrC supC = do
    spawnP (CF peerMgrC) () (catchP (openListen >>= pgm)
                        (defaultStopHandler supC)) -- TODO: Close socket resource!
  where openListen = liftIO $ listenOn port
        pgm sock = forever lp
          where lp = do
                  conn <- liftIO $ accept sock
                  syncP =<< sendPC peerMgrCh (NewIncoming conn)

