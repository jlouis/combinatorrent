module PeerMgrP (Peer(..))
where

import Network

-- A peer is an IP address and a Port.
data Peer = MkPeer { peerHost :: HostName,
                     peerPort :: PortID }
