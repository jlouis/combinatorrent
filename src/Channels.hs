{-# LANGUAGE TypeSynonymInstances #-}
module Channels
    ( Peer(..)
    , PeerChokeMsg(..)
    , MsgTy(..)
    , PeerChannel
    , MgrMessage(..)
    , MgrChannel
    , BandwidthChannel
    , TrackerMsg(..)
    , TrackerChannel
    )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq

import Network.Socket

import Protocol.Wire
import Torrent

data Peer = Peer SockAddr

data MsgTy = FromPeer (Message, Integer)
           | FromSenderQ Integer -- Always UpRate events
           | FromChokeMgr PeerChokeMsg
           | TimerTick

data PeerChokeMsg = ChokePeer
                  | UnchokePeer
                  | PieceCompleted PieceNum
                  | CancelBlock PieceNum Block

type PeerChannel = TChan MsgTy

instance NFData PeerChannel where
    rnf pc = pc `seq` ()

---- TRACKER

-- | Messages to the tracker process
data TrackerMsg = Stop -- ^ Ask the Tracker to stop
                | TrackerTick Integer -- ^ Ticking in the tracker, used to contact again
                | Start               -- ^ Ask the tracker to Start
                | Complete            -- ^ Ask the tracker to Complete the torrent
type TrackerChannel = TChan TrackerMsg

data MgrMessage = Connect InfoHash ThreadId PeerChannel
                | Disconnect ThreadId

type MgrChannel = TChan MgrMessage

-- | A Channel type we use for transferring the amount of data we transferred
type BandwidthChannel = TChan Integer
