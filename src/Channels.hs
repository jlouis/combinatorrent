module Channels
    ( Peer(..)
    , PeerMessage(..)
    , PeerChannel
    , MgrMessage(..)
    , MgrChannel
    , BandwidthChannel
    , TrackerMsg(..)
    , TrackerChannel
    )
where

import Control.Concurrent
import Control.Concurrent.CML.Strict
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

---- TRACKER

-- | Messages to the tracker process
data TrackerMsg = Stop -- ^ Ask the Tracker to stop
                | TrackerTick Integer -- ^ Ticking in the tracker, used to contact again
                | Start               -- ^ Ask the tracker to Start
                | Complete            -- ^ Ask the tracker to Complete the torrent
type TrackerChannel = Channel TrackerMsg
instance NFData TrackerMsg where
   rnf a = a `seq` ()

data MgrMessage = Connect InfoHash ThreadId PeerChannel
                | Disconnect ThreadId

instance NFData MgrMessage where
  rnf a = a `seq` ()

type MgrChannel = TChan MgrMessage

-- | A Channel type we use for transferring the amount of data we transferred
type BandwidthChannel = TChan Integer
