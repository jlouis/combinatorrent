module PeerMgrP (Peer(..),
                 start)
where

import qualified Data.Map as M
import PeerP
import Control.Concurrent
import Control.Concurrent.CML

import PeerTypes
import ConsoleP hiding (start)
import FSP hiding (start)
import Torrent hiding (infoHash)


data State = MkState { peerCh :: Channel [Peer],
                       peersInQueue  :: [Peer],
                       mgrCh :: Channel MgrMessage,
                       peers :: M.Map ThreadId (Channel PeerMessage),
                       peerId :: PeerId,
                       infoHash :: InfoHash,
                       fsCh :: FSPChannel,
                       logCh :: LogChannel }

start :: Channel [Peer] -> PeerId -> InfoHash -> FSPChannel -> LogChannel -> IO ()
start ch pid ih fsC logC = do mgrC <- channel
                              lp (MkState ch [] mgrC M.empty pid ih fsC logC )
  where lp s = (sync $ choose [trackerPeers s, peerEvent s]) >>= fillPeers >>= lp
        trackerPeers s = wrap (receive (peerCh s) (const True))
                           (\ps -> return s { peersInQueue = ps ++ (peersInQueue s) })
        peerEvent s = wrap (receive (mgrCh s) (const True))
                        (\msg ->
                             case msg of
                               Connect tid c -> newPeer s tid c
                               Disconnect tid -> removePeer s tid)
        newPeer s tid c  = do sync $ transmit c UnchokePeer
                              return s { peers = M.insert tid c (peers s)}
        removePeer s tid = return s { peers = M.delete tid (peers s) }
        fillPeers s | M.size (peers s) > 40 = return s
                    | otherwise =
                        do let (toAdd, rest) = splitAt (40 - M.size (peers s)) (peersInQueue s)
                           mapM_ (addPeer s) toAdd
                           return s { peersInQueue = rest }
        addPeer s (Peer hn prt) = PeerP.connect hn prt (peerId s) (infoHash s) (fsCh s) (logCh s) (mgrCh s)
