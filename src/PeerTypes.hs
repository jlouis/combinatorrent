module PeerTypes
    ( Peer(..)
    , PeerMessage(..)
    , PeerChannel
    , MgrMessage(..)
    , MgrChannel
    , BandwidthChannel
    )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq

import Network
import Torrent

data Peer = Peer { peerHost :: HostName,
                   peerPort :: PortID }

data PeerMessage = ChokePeer
                 | UnchokePeer
                 | PieceCompleted PieceNum
                 | CancelBlock PieceNum Block

instance NFData PeerMessage where
    rnf a = a `seq` ()

type PeerChannel = TChan PeerMessage


data MgrMessage = Connect InfoHash ThreadId PeerChannel
                | Disconnect ThreadId

instance NFData MgrMessage where
  rnf a = a `seq` ()

type MgrChannel = TChan MgrMessage

-- | A Channel type we use for transferring the amount of data we transferred
type BandwidthChannel = TChan Integer
