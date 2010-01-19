module PeerTypes
where

import Control.Concurrent
import Control.Concurrent.CML

import Network
import Torrent

data Peer = Peer { peerHost :: HostName,
                   peerPort :: PortID }

data PeerMessage = ChokePeer
                 | UnchokePeer
                 | PeerStats (Channel (Double, Bool))
		 | PieceCompleted PieceNum

type PeerChannel = Channel PeerMessage


data MgrMessage = Connect ThreadId (Channel PeerMessage)
                | Disconnect ThreadId

type MgrChannel = Channel MgrMessage
