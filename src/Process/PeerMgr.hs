module Process.PeerMgr (
   -- * Types
     Peer(..)
   , PeerMgrMsg(..)
   , PeerMgrChannel
   -- * Interface
   , start
)
where

import qualified Data.Map as M

import Control.Concurrent
import Control.Concurrent.CML.Strict
import Control.DeepSeq

import Control.Monad.State
import Control.Monad.Reader

import Network
import System.IO
import System.Log.Logger

import PeerTypes
import Process
import Process.Peer as Peer
import Process.ChokeMgr hiding (start)
import Process.FS hiding (start)
import Process.PieceMgr hiding (start)
import Process.Status hiding (start)
import Protocol.Wire

import Supervisor
import Torrent hiding (infoHash)

data PeerMgrMsg = PeersFromTracker [Peer]
                | NewIncoming (Handle, HostName, PortNumber)

instance NFData PeerMgrMsg where
  rnf a = a `seq` ()


type PeerMgrChannel = Channel PeerMgrMsg

data CF = CF { peerCh :: PeerMgrChannel
             , mgrCh :: Channel MgrMessage
             , peerPool :: SupervisorChan
             , chokeMgrCh :: ChokeMgrChannel
             }

instance Logging CF where
    logName _ = "Process.PeerMgr"

data TorrentLocal = TorrentLocal
                        { tcPcMgrCh :: PieceMgrChannel
                        , tcFSCh    :: FSPChannel
                        , tcStatCh    :: StatusChan
                        , tcPM      :: PieceMap
                        }

type ChanManageMap = M.Map InfoHash TorrentLocal

data ST = ST { peersInQueue  :: [Peer]
             , peers :: M.Map ThreadId (Channel PeerMessage)
             , peerId :: PeerId
             , infoHash :: InfoHash
             , cmMap :: ChanManageMap
             }

start :: PeerMgrChannel -> PeerId -> InfoHash -> PieceMap -> PieceMgrChannel -> FSPChannel
      -> ChokeMgrChannel -> StatusChan -> Int -> SupervisorChan
      -> IO ThreadId
start ch pid ih pm pieceMgrC fsC chokeMgrC statC nPieces supC =
    do mgrC <- channel
       fakeChan <- channel
       pool <- liftM snd $ oneForOne "PeerPool" [] fakeChan
       spawnP (CF ch mgrC pool chokeMgrC)
              (ST [] M.empty pid ih cmap) (catchP (forever lp)
                                       (defaultStopHandler supC))
  where
    cmap = M.insert ih initMMap M.empty
    initMMap = TorrentLocal pieceMgrC fsC statC pm
    lp = do chooseP [incomingPeers, peerEvent] >>= syncP
            fillPeers
    incomingPeers =
        recvWrapPC peerCh (\msg ->
            case msg of
                PeersFromTracker ps -> do
                       debugP "Adding peers to queue"
                       modify (\s -> s { peersInQueue = ps ++ peersInQueue s })
                NewIncoming conn@(h, _, _) -> do
                    sz <- liftM M.size $ gets peers
                    if sz < numPeers
                        then do debugP "New incoming peer, handling"
                                addIncoming conn
                                return ()
                        else do debugP "Already too many peers, closing!"
                                liftIO $ hClose h)
    peerEvent =
        recvWrapPC mgrCh (\msg -> case msg of
                    Connect tid c -> newPeer tid c
                    Disconnect tid -> removePeer tid)
    newPeer tid c = do debugP $ "Adding new peer " ++ show tid
                       sendPC chokeMgrCh (AddPeer tid c) >>= syncP
                       modify (\s -> s { peers = M.insert tid c (peers s)})
    removePeer tid = do debugP $ "Removing peer " ++ show tid
                        sendPC chokeMgrCh (RemovePeer tid) >>= syncP
                        modify (\s -> s { peers = M.delete tid (peers s)})
    numPeers = 40
    fillPeers = do
        sz <- liftM M.size $ gets peers
        when (sz < numPeers)
            (do q <- gets peersInQueue
                let (toAdd, rest) = splitAt (numPeers - sz) q
                debugP $ "Filling with up to " ++ show (numPeers - sz) ++ " peers"
                mapM_ addPeer toAdd
                modify (\s -> s { peersInQueue = rest }))
    addPeer (Peer hn prt) = do
        pid <- gets peerId
        ih  <- gets infoHash
        pool <- asks peerPool
        mgrC <- asks mgrCh
        cmap <- gets cmMap
        liftIO $ connect (hn, prt, pid, ih, pm) pool mgrC cmap
    addIncoming conn = do
        pid <- gets peerId
        ih  <- gets infoHash
        pool <- asks peerPool
        mgrC <- asks mgrCh
        cmap <- gets cmMap
        liftIO $ acceptor conn ih pool pid mgrC cmap

type ConnectRecord = (HostName, PortID, PeerId, InfoHash, PieceMap)

connect :: ConnectRecord -> SupervisorChan -> MgrChannel -> ChanManageMap
        -> IO ThreadId
connect (host, port, pid, ih, pm) pool mgrC cmap =
    spawn (connector >> return ())
  where tc = case M.lookup ih cmap of
                Nothing -> error "Impossible (2)"
                Just x  -> x
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
              Right (_caps, _rpid, _ih) ->
                  do debugM "Process.PeerMgr.connect" "entering peerP loop code"
                     supC <- channel -- TODO: Should be linked later on
                     children <- peerChildren h mgrC (tcPcMgrCh tc) (tcFSCh tc) (tcStatCh tc)
                                                      (tcPM tc) (M.size (tcPM tc))
                     sync $ transmit pool $ SpawnNew (Supervisor $ allForOne "PeerSup" children)
                     return ()

acceptor :: (Handle, HostName, PortNumber) -> InfoHash -> SupervisorChan
         -> PeerId -> MgrChannel -> ChanManageMap
         -> IO ThreadId
acceptor (h,hn,pn) ih pool pid mgrC cmmap =
    spawn (connector >> return ())
  where ihTst k = M.member k cmmap
        tc = case M.lookup ih cmmap of
                Nothing -> error "Impossible"
                Just x  -> x
        connector = do
            debugLog "Handling incoming connection"
            r <- receiveHandshake h pid ihTst ih
            debugLog "RecvHandshake run"
            case r of
                Left err -> do debugLog ("Incoming Peer handshake failure with " ++ show hn ++ "("
                                            ++ show pn ++ "), error: " ++ err)
                               return ()
                Right (_caps, _rpid, ih) ->
                    do debugLog "entering peerP loop code"
                       supC <- channel -- TODO: Should be linked later on
                       children <- peerChildren h mgrC (tcPcMgrCh tc) (tcFSCh tc)
                                                        (tcStatCh tc) (tcPM tc) (M.size (tcPM tc))
                       sync $ transmit pool $ SpawnNew (Supervisor $ allForOne "PeerSup" children)
                       return ()
        debugLog = debugM "Process.PeerMgr.acceptor"

showPort :: PortID -> String
showPort (PortNumber pn) = show pn
showPort _               = "N/A"
