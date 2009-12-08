module PeerMgrP (Peer(..))
where

-- A peer is an IP address and a Port.
data Peer = MkPeer { peerIP :: (Int, Int, Int, Int),
                     peerPort :: Int }
