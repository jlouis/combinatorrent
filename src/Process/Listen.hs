module Process.Listen
    ( start
    )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader

import Network
import Network.Socket as S

import Process
import Process.PeerMgr hiding (start)
import Supervisor

data CF = CF { peerMgrCh :: PeerMgrChannel }

instance Logging CF where
    logName _ = "Process.Listen"

start :: PortNumber -> PeerMgrChannel -> SupervisorChannel -> IO ThreadId
start port peerMgrC supC = do
    spawnP (CF peerMgrC) () (catchP (openListen port >>= eventLoop)
                        (defaultStopHandler supC)) -- TODO: Close socket resource!

openListen :: PortNumber -> Process CF () Socket
openListen port = liftIO $ do
    s <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.bindSocket s (S.SockAddrInet port S.iNADDR_ANY)
    S.listen s 10
    return s

eventLoop :: Socket -> Process CF () ()
eventLoop sockFd = do
    c <- asks peerMgrCh
    liftIO $ do
        conn <- S.accept sockFd
        atomically $ writeTChan c (NewIncoming conn)
    eventLoop sockFd

