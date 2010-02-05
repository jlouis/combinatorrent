module PeerTypes
where

import Control.Concurrent
import Control.Concurrent.CML

import Data.Time.Clock

import Network
import Torrent

data Peer = Peer { peerHost :: HostName,
                   peerPort :: PortID }

data PeerMessage = ChokePeer
                 | UnchokePeer
                 | PeerStats UTCTime (Channel (Double, Double, Bool)) -- Up/Down/Interested
		 | PieceCompleted PieceNum
		 | CancelBlock PieceNum Block

type PeerChannel = Channel PeerMessage


data MgrMessage = Connect ThreadId (Channel PeerMessage)
                | Disconnect ThreadId

type MgrChannel = Channel MgrMessage

-- | A Channel type we use for transferring the amount of data we transferred
type BandwidthChannel = Channel Integer
