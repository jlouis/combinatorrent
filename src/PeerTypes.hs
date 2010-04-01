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
import Control.Concurrent.CML.Strict
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

type PeerChannel = Channel PeerMessage


data MgrMessage = Connect InfoHash ThreadId (Channel PeerMessage)
                | Disconnect ThreadId

instance NFData MgrMessage where
  rnf a = a `seq` ()

type MgrChannel = Channel MgrMessage

-- | A Channel type we use for transferring the amount of data we transferred
type BandwidthChannel = Channel Integer
