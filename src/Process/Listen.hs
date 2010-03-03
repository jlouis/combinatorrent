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
import Logging
import Supervisor

data CF = CF { peerMgrCh :: PeerMgrChannel
             , logCh :: LogChannel
             }

instance Logging CF where
    getLogger cf = ("ListenP", logCh cf)

start :: PortID -> PeerMgrChannel -> LogChannel -> SupervisorChan -> IO ThreadId
start port peerMgrC logC supC = do
    spawnP (CF peerMgrC logC) () (catchP (openListen >>= pgm)
                        (defaultStopHandler supC)) -- TODO: Close socket resource!
  where openListen = liftIO $ listenOn port
        pgm sock = forever lp
          where lp = do
                  conn <- liftIO $ accept sock
                  syncP =<< sendPC peerMgrCh (NewIncoming conn)

