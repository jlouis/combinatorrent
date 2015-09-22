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

import Control.Applicative

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq

import Control.Monad.State
import Control.Monad.Reader

import Data.Array
import qualified Data.Map as M

import qualified Network.Socket as Sock
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
                | NewIncoming (Sock.Socket, Sock.SockAddr)
                | NewTorrent InfoHash TorrentLocal
                | StopTorrent InfoHash

data TorrentLocal = TorrentLocal
                        { tcPcMgrCh :: !PieceMgrChannel
                        , tcFSCh    :: !FSPChannel
                        , tcStatTV  :: !(TVar [PStat])
                        , tcPM      :: !PieceMap
                        }

#if __GLASGOW_HASKELL__ < 710
instance NFData ThreadId where
    rnf x = x `seq` ()
#endif

type PeerMgrChannel = TChan PeerMgrMsg

data CF = CF { peerCh :: PeerMgrChannel
             , mgrCh :: MgrChannel
             , peerPool :: SupervisorChannel
             , chokeMgrCh :: ChokeMgrChannel
             , chokeRTV :: RateTVar
             }

instance Logging CF where
    logName _ = "Process.PeerMgr"


type ChanManageMap = M.Map InfoHash TorrentLocal

data ST = ST { peersInQueue  :: ![(InfoHash, Peer)]
             , peers ::         !(M.Map ThreadId PeerChannel)
             , peerId ::        !PeerId
             , cmMap ::         !ChanManageMap
             }

start :: PeerMgrChannel -> PeerId
      -> ChokeMgrChannel -> RateTVar -> SupervisorChannel
      -> IO ThreadId
start ch pid chokeMgrC rtv supC =
    do mgrC <- newTChanIO
       fakeChan <- newTChanIO
       pool <- liftM snd $ oneForOne "PeerPool" [] fakeChan
       spawnP (CF ch mgrC pool chokeMgrC rtv)
              (ST [] M.empty pid cmap) ({-# SCC "PeerMgr" #-} catchP lp
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
        lp

incomingPeers :: PeerMgrMsg -> Process CF ST ()
incomingPeers msg =
   case msg of
       PeersFromTracker ih ps -> do
              debugP "Adding peers to queue"
              modify (\s -> s { peersInQueue = (map (ih,) ps) ++ peersInQueue s })
       NewIncoming conn@(s, _) -> do
           sz <- liftM M.size $ gets peers
           if sz < numPeers
               then do debugP "New incoming peer, handling"
                       _ <- addIncoming conn
                       return ()
               else do debugP "Already too many peers, closing!"
                       liftIO $ Sock.sClose s
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
                          npeers <- M.insert tid c <$> gets peers
                          npeers `deepseq` modify (\s -> s { peers = npeers })
    removePeer tid = do debugP $ "Removing peer " ++ show tid
                        cch <- asks chokeMgrCh
                        liftIO . atomically $ writeTChan cch (RemovePeer tid)
                        npeers <- M.delete tid <$> gets peers
                        npeers `deepseq` modify (\s -> s { peers = npeers })

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
addPeer (ih, (Peer addr)) = do
    ppid <- gets peerId
    pool <- asks peerPool
    mgrC <- asks mgrCh
    cm   <- gets cmMap
    v    <- asks chokeRTV
    liftIO $ connect (addr, ppid, ih) pool mgrC v cm

addIncoming :: (Sock.Socket, Sock.SockAddr) -> Process CF ST ThreadId
addIncoming conn = do
    ppid   <- gets peerId
    pool <- asks peerPool
    mgrC <- asks mgrCh
    v    <- asks chokeRTV
    cm   <- gets cmMap
    liftIO $ acceptor conn pool ppid mgrC v cm

type ConnectRecord = (Sock.SockAddr, PeerId, InfoHash)

connect :: ConnectRecord -> SupervisorChannel -> MgrChannel -> RateTVar -> ChanManageMap
        -> IO ThreadId
connect (addr, pid, ih) pool mgrC rtv cmap =
    forkIO (connector >> return ())
  where 
        connector = {-# SCC "connect" #-}
         do sock <- Sock.socket Sock.AF_INET Sock.Stream Sock.defaultProtocol
            debugM "Process.PeerMgr.connect" $ "Connecting to: " ++ show addr
            Sock.connect sock addr
            debugM "Process.PeerMgr.connect" "Connected, initiating handShake"
            r <- initiateHandshake sock pid ih
            debugM "Process.PeerMgr.connect" "Handshake run"
            case r of
              Left err -> do debugM "Process.PeerMgr.connect"
                                ("Peer handshake failure at host " ++ show addr
                                  ++ " with error " ++ err)
                             return ()
              Right (caps, _rpid, ihsh) ->
                  do debugM "Process.PeerMgr.connect" "entering peerP loop code"
                     let tc = case M.lookup ihsh cmap of
                                    Nothing -> error "Impossible (2), I hope"
                                    Just x  -> x
                     children <- Peer.start sock caps mgrC rtv
                                                      (tcPcMgrCh tc) (tcFSCh tc) (tcStatTV tc)
                                                      (tcPM tc) (succ . snd . bounds $ tcPM tc)
                                                      ihsh
                     atomically $ writeTChan pool $
                        SpawnNew (Supervisor $ allForOne "PeerSup" children)
                     return ()

acceptor :: (Sock.Socket, Sock.SockAddr) -> SupervisorChannel
         -> PeerId -> MgrChannel -> RateTVar -> ChanManageMap
         -> IO ThreadId
acceptor (s,sa) pool pid mgrC rtv cmmap =
    forkIO (connector >> return ())
  where ihTst k = M.member k cmmap
        connector = {-# SCC "acceptor" #-} do
            debugLog "Handling incoming connection"
            r <- receiveHandshake s pid ihTst
            debugLog "RecvHandshake run"
            case r of
                Left err -> do debugLog ("Incoming Peer handshake failure with "
                                            ++ show sa ++ ", error: " ++ err)
                               return()
                Right (caps, _rpid, ih) ->
                    do debugLog "entering peerP loop code"
                       let tc = case M.lookup ih cmmap of
                                  Nothing -> error "Impossible, I hope"
                                  Just x  -> x
                       children <- Peer.start s caps mgrC rtv (tcPcMgrCh tc) (tcFSCh tc)
                                                        (tcStatTV tc) (tcPM tc)
                                                        (succ . snd . bounds $ tcPM tc) ih
                       atomically $ writeTChan pool $
                            SpawnNew (Supervisor $ allForOne "PeerSup" children)
                       return ()
        debugLog = debugM "Process.PeerMgr.acceptor"

