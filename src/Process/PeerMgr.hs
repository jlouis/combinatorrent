{-# LANGUAGE TupleSections #-}
module Process.PeerMgr (
   -- * Types
     Peer(..)
   , PeerMgrMsg(..)
   , PeerMgrChannel
   , TorrentLocal(..)
   -- * Interface
   , Process.PeerMgr.start
)
where

import qualified Data.Map as M

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq

import Control.Monad.State
import Control.Monad.Reader

import Network
import System.IO
import System.Log.Logger

import Channels
import Process
import Process.Peer as Peer
import Process.ChokeMgr hiding (start)
import Process.FS hiding (start)
import Process.PieceMgr hiding (start)
import Process.Status hiding (start)
import Protocol.Wire

import Supervisor
import Torrent hiding (infoHash)

data PeerMgrMsg = PeersFromTracker InfoHash [Peer]
                | NewIncoming (Handle, HostName, PortNumber)
                | NewTorrent InfoHash TorrentLocal
                | StopTorrent InfoHash

instance NFData PeerMgrMsg where
  rnf a = a `seq` ()

data TorrentLocal = TorrentLocal
                        { tcPcMgrCh :: PieceMgrChannel
                        , tcFSCh    :: FSPChannel
                        , tcStatTV  :: TVar [PStat]
                        , tcPM      :: PieceMap
                        }



type PeerMgrChannel = TChan PeerMgrMsg

data CF = CF { peerCh :: PeerMgrChannel
             , mgrCh :: MgrChannel
             , peerPool :: SupervisorChan
             , chokeMgrCh :: ChokeMgrChannel
             , chokeRTV :: RateTVar
             }

instance Logging CF where
    logName _ = "Process.PeerMgr"


type ChanManageMap = M.Map InfoHash TorrentLocal

data ST = ST { peersInQueue  :: [(InfoHash, Peer)]
             , peers :: M.Map ThreadId PeerChannel
             , peerId :: PeerId
             , cmMap :: ChanManageMap
             }

start :: PeerMgrChannel -> PeerId
      -> ChokeMgrChannel -> RateTVar -> SupervisorChan
      -> IO ThreadId
start ch pid chokeMgrC rtv supC =
    do mgrC <- newTChanIO
       fakeChan <- newTChanIO
       pool <- liftM snd $ oneForOne "PeerPool" [] fakeChan
       spawnP (CF ch mgrC pool chokeMgrC rtv)
              (ST [] M.empty pid cmap) (catchP (forever lp)
                                       (defaultStopHandler supC))
  where
    cmap = M.empty
    lp = do
        pc <- asks peerCh
        mc <- asks mgrCh
        q <- liftIO . atomically $
                    (readTChan pc >>= return . Left) `orElse`
                    (readTChan mc >>= return . Right)
        case q of
            Left msg -> incomingPeers msg
            Right msg -> peerEvent msg
        fillPeers

incomingPeers :: PeerMgrMsg -> Process CF ST ()
incomingPeers msg =
   case msg of
       PeersFromTracker ih ps -> do
              debugP "Adding peers to queue"
              modify (\s -> s { peersInQueue = (map (ih,) ps) ++ peersInQueue s })
       NewIncoming conn@(h, _, _) -> do
           sz <- liftM M.size $ gets peers
           if sz < numPeers
               then do debugP "New incoming peer, handling"
                       _ <- addIncoming conn
                       return ()
               else do debugP "Already too many peers, closing!"
                       liftIO $ hClose h
       NewTorrent ih tl -> do
           modify (\s -> s { cmMap = M.insert ih tl (cmMap s)})
       StopTorrent _ih -> do
           errorP "Not implemented stopping yet"

peerEvent :: MgrMessage -> Process CF ST ()
peerEvent msg = case msg of
                  Connect ih tid c -> newPeer ih tid c
                  Disconnect tid -> removePeer tid
  where
    newPeer ih tid c = do debugP $ "Adding new peer " ++ show tid
                          cch <- asks chokeMgrCh
                          liftIO . atomically $ writeTChan cch (AddPeer ih tid c)
                          modify (\s -> s { peers = M.insert tid c (peers s)})
    removePeer tid = do debugP $ "Removing peer " ++ show tid
                        cch <- asks chokeMgrCh
                        liftIO . atomically $ writeTChan cch (RemovePeer tid)
                        modify (\s -> s { peers = M.delete tid (peers s)})

numPeers :: Int
numPeers = 40

fillPeers :: Process CF ST ()
fillPeers = do
    sz <- liftM M.size $ gets peers
    when (sz < numPeers)
        (do q <- gets peersInQueue
            let (toAdd, rest) = splitAt (numPeers - sz) q
            debugP $ "Filling with up to " ++ show (numPeers - sz) ++ " peers"
            mapM_ addPeer toAdd
            modify (\s -> s { peersInQueue = rest }))

addPeer :: (InfoHash, Peer) -> Process CF ST ThreadId
addPeer (ih, (Peer hn prt)) = do
    ppid <- gets peerId
    pool <- asks peerPool
    mgrC <- asks mgrCh
    cm   <- gets cmMap
    v    <- asks chokeRTV
    liftIO $ connect (hn, prt, ppid, ih) pool mgrC v cm

addIncoming :: (Handle, HostName, PortNumber) -> Process CF ST ThreadId
addIncoming conn = do
    ppid   <- gets peerId
    pool <- asks peerPool
    mgrC <- asks mgrCh
    v    <- asks chokeRTV
    cm   <- gets cmMap
    liftIO $ acceptor conn pool ppid mgrC v cm

type ConnectRecord = (HostName, PortID, PeerId, InfoHash)

connect :: ConnectRecord -> SupervisorChan -> MgrChannel -> RateTVar -> ChanManageMap
        -> IO ThreadId
connect (host, port, pid, ih) pool mgrC rtv cmap =
    forkIO (connector >> return ())
  where 
        connector =
         do debugM "Process.PeerMgr.connect" $
                "Connecting to " ++ show host ++ " (" ++ showPort port ++ ")"
            h <- connectTo host port
            debugM "Process.PeerMgr.connect" "Connected, initiating handShake"
            r <- initiateHandshake h pid ih
            debugM "Process.PeerMgr.connect" "Handshake run"
            case r of
              Left err -> do debugM "Process.PeerMgr.connect"
                                ("Peer handshake failure at host " ++ host
                                  ++ " with error " ++ err)
                             return ()
              Right (_caps, _rpid, ihsh) ->
                  do debugM "Process.PeerMgr.connect" "entering peerP loop code"
                     let tc = case M.lookup ihsh cmap of
                                    Nothing -> error "Impossible (2), I hope"
                                    Just x  -> x
                     children <- Peer.start h mgrC rtv (tcPcMgrCh tc) (tcFSCh tc) (tcStatTV tc)
                                                      (tcPM tc) (M.size (tcPM tc)) ihsh
                     atomically $ writeTChan pool $
                        SpawnNew (Supervisor $ allForOne "PeerSup" children)
                     return ()

acceptor :: (Handle, HostName, PortNumber) -> SupervisorChan
         -> PeerId -> MgrChannel -> RateTVar -> ChanManageMap
         -> IO ThreadId
acceptor (h,hn,pn) pool pid mgrC rtv cmmap =
    forkIO (connector >> return ())
  where ihTst k = M.member k cmmap
        connector = do
            debugLog "Handling incoming connection"
            r <- receiveHandshake h pid ihTst
            debugLog "RecvHandshake run"
            case r of
                Left err -> do debugLog ("Incoming Peer handshake failure with " ++ show hn ++ "("
                                            ++ show pn ++ "), error: " ++ err)
                               return ()
                Right (_caps, _rpid, ih) ->
                    do debugLog "entering peerP loop code"
                       let tc = case M.lookup ih cmmap of
                                  Nothing -> error "Impossible, I hope"
                                  Just x  -> x
                       children <- Peer.start h mgrC rtv (tcPcMgrCh tc) (tcFSCh tc)
                                                        (tcStatTV tc) (tcPM tc) (M.size (tcPM tc)) ih
                       atomically $ writeTChan pool $
                            SpawnNew (Supervisor $ allForOne "PeerSup" children)
                       return ()
        debugLog = debugM "Process.PeerMgr.acceptor"

showPort :: PortID -> String
showPort (PortNumber pn) = show pn
showPort _               = "N/A"
