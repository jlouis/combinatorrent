-- | The Manager Process - Manages the torrents and controls them
module Process.TorrentManager (
    -- * Types
      TorrentManagerMsg(..)
    -- * Channels
    , TorrentMgrChan
    -- * Interface
    , start
    )
where

import Control.Concurrent
import Control.Concurrent.CML.Strict
import Control.Concurrent.STM
import Control.DeepSeq

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.ByteString as B
import Prelude hiding (log)

import Protocol.BCode as BCode
import Process
import qualified Process.Status as Status
import qualified Process.PeerMgr as PeerMgr
import qualified Process.FS as FSP
import qualified Process.PieceMgr as PieceMgr (start, createPieceDb)
import qualified Process.ChokeMgr as ChokeMgr (ChokeMgrChannel)
import qualified Process.Tracker as Tracker
import FS
import Supervisor
import Torrent

data TorrentManagerMsg = AddedTorrent FilePath
                       | RemovedTorrent FilePath
  deriving (Eq, Show)

instance NFData TorrentManagerMsg where
  rnf a = a `seq` ()

type TorrentMgrChan = Channel [TorrentManagerMsg]

data CF = CF { tCh :: TorrentMgrChan
             , tStatusCh    :: Status.StatusChan
             , tStatusTV    :: TVar [Status.PStat]
             , tPeerId      :: PeerId
             , tPeerMgrCh   :: PeerMgr.PeerMgrChannel
             , tChokeCh     :: ChokeMgr.ChokeMgrChannel
             }

instance Logging CF where
  logName _ = "Process.TorrentManager"

data ST = ST { workQueue :: [TorrentManagerMsg] }
start :: TorrentMgrChan -- ^ Channel to watch for changes to torrents
      -> Status.StatusChan
      -> TVar [Status.PStat]
      -> ChokeMgr.ChokeMgrChannel
      -> PeerId
      -> PeerMgr.PeerMgrChannel
      -> SupervisorChan
      -> IO ThreadId
start chan statusC stv chokeC pid peerC supC =
    spawnP (CF chan statusC stv pid peerC chokeC) (ST [])
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
                    _ <- startTorrent fp
                    modify (\s -> s { workQueue = rest })
                    startStop
                (RemovedTorrent _ : _) -> do
                    errorP "Removal of torrents not yet supported :P"
                    stopP

readTorrent :: FilePath -> Process CF ST BCode
readTorrent fp = do
    torrent <- liftIO $ B.readFile fp
    let bcoded = BCode.decode torrent
    case bcoded of
      Left err -> do liftIO $ print err
                     stopP
      Right bc -> return bc

startTorrent :: FilePath -> Process CF ST ThreadId
startTorrent fp = do
    bc <- readTorrent fp
    fspC     <- liftIO newTChanIO
    trackerC <- liftIO channel
    supC     <- liftIO channel
    pieceMgrC  <- liftIO newTChanIO
    chokeC  <- asks tChokeCh
    statusC <- asks tStatusCh
    stv <- asks tStatusTV
    pid     <- asks tPeerId
    pmC     <- asks tPeerMgrCh
    (handles, haveMap, pieceMap) <- liftIO $ openAndCheckFile bc
    let left = bytesLeft haveMap pieceMap
    ti <- liftIO $ mkTorrentInfo bc
    pieceDb <- PieceMgr.createPieceDb haveMap pieceMap
    tid <- liftIO $ allForOne ("TorrentSup - " ++ fp)
                     [ Worker $ FSP.start handles pieceMap fspC
                     , Worker $ PieceMgr.start pieceMgrC fspC chokeC statusC pieceDb (infoHash ti)
                     , Worker $ Tracker.start (infoHash ti) ti pid defaultPort statusC trackerC pmC
                     ] supC
    syncP =<< (sendP statusC $ Status.InsertTorrent (infoHash ti) left trackerC)
    syncP =<< (sendPC tPeerMgrCh $ PeerMgr.NewTorrent (infoHash ti)
                            (PeerMgr.TorrentLocal pieceMgrC fspC stv pieceMap ))
    syncP =<< sendP trackerC Status.Start
    return tid