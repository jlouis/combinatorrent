module PeerMgrP (
   -- * Types
     Peer(..)
   -- * Interface
   , start
)
where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S

import Control.Concurrent
import Control.Concurrent.CML
import Control.Monad

import System.Random

import ChokeMgrP hiding (start)
import PeerP
import PeerTypes
import PieceMgrP hiding (start)
import ConsoleP hiding (start)
import FSP hiding (start)
import Supervisor
import Torrent hiding (infoHash)


data State = MkState { peerCh :: Channel [Peer],
                       pieceMgrCh :: PieceMgrChannel,
                       peersInQueue  :: [Peer],
                       mgrCh :: Channel MgrMessage,
                       peers :: M.Map ThreadId (Channel PeerMessage),
                       peerId :: PeerId,
                       infoHash :: InfoHash,
                       fsCh :: FSPChannel,
                       peerPool :: SupervisorChan,
                       logCh :: LogChannel,
		       weSeeding :: Bool -- ^ True if we are currently seeding the torrent
		       }

start :: Channel [Peer] -> PeerId -> InfoHash -> PieceMap -> PieceMgrChannel -> FSPChannel -> LogChannel -> ChokeMgrChannel -> Int -> SupervisorChan -> IO ThreadId
start ch pid ih pm pieceMgrC fsC logC chokeMgrC nPieces supC =
    do mgrC <- channel
       fakeChan <- channel
       pool <- liftM snd $ oneForOne [] fakeChan
       spawn $ startup (MkState ch pieceMgrC [] mgrC M.empty pid ih fsC pool logC False)
  where startup s = Supervisor.defaultStartup supC "PeerMgr" (lp s)
	lp s = do logMsg logC "Looping PeerMgr"
                  sync (choose [trackerPeers s, peerEvent s]) >>= fillPeers >>= lp
        trackerPeers s = wrap (receive (peerCh s) (const True))
                           (\ps ->
                                do logMsg (logCh s) "Adding peers to queue"
                                   return s { peersInQueue = ps ++ peersInQueue s })
        peerEvent s = wrap (receive (mgrCh s) (const True))
                        (\msg ->
                             case msg of
                               Connect tid c -> newPeer s tid c
                               Disconnect tid -> removePeer s tid)
        newPeer s tid c  = do logMsg (logCh s) "Unchoking new peer"
			      ChokeMgrP.addPeer chokeMgrC tid c (weSeeding s)
                              return s { peers = M.insert tid c (peers s)}
        removePeer s tid = do logMsg (logCh s) "Deleting peer"
			      ChokeMgrP.removePeer chokeMgrC tid
                              return s { peers = M.delete tid (peers s) }
        fillPeers s | M.size (peers s) > 40 = return s
                    | otherwise =
                        do let (toAdd, rest) = splitAt (40 - M.size (peers s)) (peersInQueue s)
                           logMsg (logCh s) $ "Filling with up to " ++ show (40 - M.size (peers s)) ++ " peers"
                           mapM_ (addPeer s) toAdd
                           return s { peersInQueue = rest }
        addPeer s (Peer hn prt) = do
          logMsg (logCh s) "Adding peer"
	  PeerP.connect (hn, prt, peerId s, infoHash s, pm) (peerPool s) (pieceMgrCh s) (fsCh s) (logCh s) (mgrCh s) nPieces
          logMsg (logCh s) "... Added"

