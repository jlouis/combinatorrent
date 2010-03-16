-- | The Manager Process - Manages the torrents and controls them
module Process.TorrentManager (
    -- * Types
    -- * Channels
    -- * Interface
    start
    )
where

import Control.Concurrent
import Control.Concurrent.CML.Strict

import Control.Monad.State

import Prelude hiding (log)
import Process
import Process.PieceMgr (ChokeInfoChannel)
import qualified Process.Status as Status
import qualified Process.PeerMgr as PeerMgr
import Process.DirWatcher (DirWatchChan, DirWatchMsg(..))

import Torrent

import Supervisor

data CF = CF { tCh :: DirWatchChan
             , tChokeInfoCh :: ChokeInfoChannel
             , tStatusCh    :: Channel Status.ST
             , tPeerId      :: PeerId
             , tPeerMgrCh   :: PeerMgr.PeerMgrChannel
             }

instance Logging CF where
  logName _ = "Process.TorrentManager"

data ST = ST { workQueue :: [DirWatchMsg] }
start :: DirWatchChan -- ^ Channel to watch for changes to torrents
      -> ChokeInfoChannel
      -> Channel Status.ST
      -> PeerId
      -> PeerMgr.PeerMgrChannel
      -> SupervisorChan
      -> IO ThreadId
start chan chokeInfoC statusC pid peerC supC =
    spawnP (CF chan chokeInfoC statusC pid peerC) (ST [])
                (catchP (forever pgm) (defaultStopHandler supC))
  where pgm = do startStop >> (syncP =<< chooseP [dirEvt])
        dirEvt =
            recvWrapPC tCh
                (\ls -> modify (\s -> s { workQueue = ls ++ workQueue s}))
        startStop = do
            q <- gets workQueue
            case q of
                [] -> return ()
                (AddedTorrent fp : rest) -> do
                    debugP $ "Adding torrent file: " ++ fp
                    modify (\s -> s { workQueue = rest })
                (RemovedTorrent fp : _) -> do
                    errorP "Removal of torrents not yet supported :P"
                    stopP
