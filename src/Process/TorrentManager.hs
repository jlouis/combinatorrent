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
             , tStatusCh    :: Channel Status.ST
             , tPeerId      :: PeerId
             , tPeerMgrCh   :: PeerMgr.PeerMgrChannel
             }

instance Logging CF where
  logName _ = "Process.TorrentManager"

data ST = ST { workQueue :: [TorrentManagerMsg] }
start :: TorrentMgrChan -- ^ Channel to watch for changes to torrents
      -> Channel Status.ST
      -> PeerId
      -> PeerMgr.PeerMgrChannel
      -> SupervisorChan
      -> IO ThreadId
start chan statusC pid peerC supC =
    spawnP (CF chan statusC pid peerC) (ST [])
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
                    startTorrent fp
                    modify (\s -> s { workQueue = rest })
                    startStop
                (RemovedTorrent fp : _) -> do
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
    fspC     <- liftIO channel
    trackerC <- liftIO channel
    supC     <- liftIO channel
    chokeInfoC <- liftIO channel
    statInC    <- liftIO channel
    pieceMgrC  <- liftIO channel
    statusC <- asks tStatusCh
    pid     <- asks tPeerId
    pmC     <- asks tPeerMgrCh
    (handles, haveMap, pieceMap) <- liftIO $ openAndCheckFile bc
    let left = bytesLeft haveMap pieceMap
        clientState = determineState haveMap
    ti <- liftIO $ mkTorrentInfo bc
    tid <- liftIO $ allForOne ("TorrentSup - " ++ fp)
                     [ Worker $ FSP.start handles pieceMap fspC
                     , Worker $ PieceMgr.start pieceMgrC fspC chokeInfoC statInC
                                        (PieceMgr.createPieceDb haveMap pieceMap)
                     , Worker $ Status.start left clientState statusC statInC trackerC
                     , Worker $ Tracker.start (infoHash ti) ti pid defaultPort statusC statInC
                                        trackerC pmC
                     ] supC
    syncP =<< (sendPC tPeerMgrCh $ PeerMgr.NewTorrent (infoHash ti)
                            (PeerMgr.TorrentLocal pieceMgrC fspC statInC pieceMap ))
    syncP =<< sendP trackerC Status.Start
    return tid
