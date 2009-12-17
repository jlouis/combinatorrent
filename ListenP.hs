module ListenP
where

import Control.Concurrent.CML

import Network

import ConsoleP
import PeerP
import Torrent
import FSP

start :: PortID -> PeerId -> InfoHash -> FSPChannel -> LogChannel -> IO ()
start port pid ih fsC logC =
    do sock <- listenOn port
       spawn $ acceptor sock pid ih fsC logC
       return ()

acceptor :: Socket -> PeerId -> InfoHash -> FSPChannel -> LogChannel -> IO ()
acceptor sock pid ih fsC logC =
    do (h, _, _) <- accept sock
       spawn $ acceptor sock pid ih fsC logC
       r <- PeerP.listenHandshake h pid ih fsC logC
       case r of
         Left err -> do logMsg logC $ "Incoming peer Accept error" ++ err
                        return ()
         Right () -> return ()


