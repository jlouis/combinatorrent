module TrackerP

where

-- import Control.Concurrent
import Control.Concurrent.Chan

data Tasks = NeedPeers -- Don't fret too much, just ask the tracker

data Peer = MkPeer { peerIP :: String,
                     peerPort :: Int }

spawn :: String -> Chan Peer -> Chan Tasks -> IO ()
spawn announce _peerC _taskC =
    putStrLn ("Spawned tracker process for " ++ announce)
