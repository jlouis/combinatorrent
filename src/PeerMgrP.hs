module PeerMgrP (
   -- * Types
     Peer(..)
   -- * Interface
   , start
)
where

import Data.List
import qualified Data.Map as M
import Data.Ord

import Control.Concurrent
import Control.Concurrent.CML
import Control.Monad.State
import Control.Monad.Reader

import ChokeMgrP hiding (start)
import PeerP
import PeerTypes
import PieceMgrP hiding (start)
import Process
import Logging
import FSP hiding (start)
import StatusP hiding (start)
import Supervisor
import Torrent hiding (infoHash)


data CF = CF { peerCh :: Channel [Peer]
	     , pieceMgrCh :: PieceMgrChannel
	     , mgrCh :: Channel MgrMessage
	     , fsCh  :: FSPChannel
	     , peerPool :: SupervisorChan
	     , chokeMgrCh :: ChokeMgrChannel
	     , logCh :: LogChannel
	     }

instance Logging CF where
  getLogger cf = ("PeerMgrP", logCh cf)

data ST = ST { peersInQueue  :: [Peer]
             , peers :: M.Map ThreadId (Channel PeerMessage)
             , peerId :: PeerId
             , infoHash :: InfoHash
	     }

start :: Channel [Peer] -> PeerId -> InfoHash -> PieceMap -> PieceMgrChannel -> FSPChannel
      -> LogChannel -> ChokeMgrChannel -> StatusChan -> Int -> SupervisorChan
      -> IO ThreadId
start ch pid ih pm pieceMgrC fsC logC chokeMgrC statC nPieces supC =
    do mgrC <- channel
       fakeChan <- channel
       pool <- liftM snd $ oneForOne "PeerPool" [] logC fakeChan
       spawnP (CF ch pieceMgrC mgrC fsC pool chokeMgrC logC)
              (ST [] M.empty pid ih) (catchP (forever lp)
	                               (defaultStopHandler supC))
  where
    lp = do chooseP [trackerPeers, peerEvent] >>= syncP
	    fillPeers
    trackerPeers = do
	ev <- recvPC peerCh
	wrapP ev (\ps ->
	    do logDebug "Adding peers to queue"
	       modify (\s -> s { peersInQueue = ps ++ peersInQueue s }))
    peerEvent = do
	ev <- recvPC mgrCh
	wrapP ev (\msg -> do
		case msg of
		    Connect tid c -> newPeer tid c
		    Disconnect tid -> removePeer tid)
    newPeer tid c = do logDebug $ "Adding new peer " ++ show tid
		       sendPC chokeMgrCh (AddPeer tid c) >>= syncP
		       modify (\s -> s { peers = M.insert tid c (peers s)})
    removePeer tid = do logDebug $ "Removing peer " ++ show tid
		        sendPC chokeMgrCh (RemovePeer tid) >>= syncP
			modify (\s -> s { peers = M.delete tid (peers s)})
    numPeers = 40
    fillPeers = do
	sz <- liftM M.size $ gets peers
	when (sz < numPeers)
	    (do q <- gets peersInQueue
		let (toAdd, rest) = splitAt (numPeers - sz) q
		logDebug $ "Filling with up to " ++ show (numPeers - sz) ++ " peers"
		mapM_ addPeer toAdd
		modify (\s -> s { peersInQueue = rest }))
    addPeer (Peer hn prt) = do
	pid <- gets peerId
	ih  <- gets infoHash
	pool <- asks peerPool
	pmC  <- asks pieceMgrCh
	fsC  <- asks fsCh
	mgrC <- asks mgrCh
	logC <- asks logCh
	liftIO $ PeerP.connect (hn, prt, pid, ih, pm) pool pmC fsC logC statC mgrC nPieces
