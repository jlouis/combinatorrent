module PeerMgrP (Peer(..),
                 start)
where

import Control.Concurrent.CML

import Network

-- A peer is an IP address and a Port.
data Peer = MkPeer { peerHost :: HostName,
                     peerPort :: PortID }

data State = MkState { peerCh :: Channel [Peer],
                       peers  :: [Peer] }

start :: Channel [Peer] -> IO ()
start ch = do lp (MkState ch [])
  where lp s = do ps <- sync $ receive (peerCh s) (const True)
                  lp s { peers = ps ++ (peers s) }
