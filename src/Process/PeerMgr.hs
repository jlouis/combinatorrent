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
import Control.Concurrent.CML
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

type PeerMgrChannel = Channel PeerMgrMsg

data CF = CF { peerCh :: PeerMgrChannel
             , pieceMgrCh :: PieceMgrChannel
             , mgrCh :: Channel MgrMessage
             , fsCh  :: FSPChannel
             , peerPool :: SupervisorChan
             , chokeMgrCh :: ChokeMgrChannel
             }

instance Logging CF where
    logName _ = "Process.PeerMgr"

data ST = ST { peersInQueue  :: [Peer]
             , peers :: M.Map ThreadId (Channel PeerMessage)
             , peerId :: PeerId
             , infoHash :: InfoHash
             }

start :: PeerMgrChannel -> PeerId -> InfoHash -> PieceMap -> PieceMgrChannel -> FSPChannel
      -> ChokeMgrChannel -> StatusChan -> Int -> SupervisorChan
      -> IO ThreadId
start ch pid ih pm pieceMgrC fsC chokeMgrC statC nPieces supC =
    do mgrC <- channel
       fakeChan <- channel
       pool <- liftM snd $ oneForOne "PeerPool" [] fakeChan
       spawnP (CF ch pieceMgrC mgrC fsC pool chokeMgrC)
              (ST [] M.empty pid ih) (catchP (forever lp)
                                       (defaultStopHandler supC))
  where
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
        pmC  <- asks pieceMgrCh
        fsC  <- asks fsCh
        mgrC <- asks mgrCh
        liftIO $ connect (hn, prt, pid, ih, pm) pool pmC fsC statC mgrC nPieces
    addIncoming conn = do
        pid <- gets peerId
        ih  <- gets infoHash
        pool <- asks peerPool
        pieceMgrC  <- asks pieceMgrCh
        fsC  <- asks fsCh
        mgrC <- asks mgrCh
        let ihTst = (== ih)
        liftIO $ acceptor conn ih ihTst pool pid mgrC pieceMgrC fsC statC pm nPieces

type ConnectRecord = (HostName, PortID, PeerId, InfoHash, PieceMap)

connect :: ConnectRecord -> SupervisorChan -> PieceMgrChannel -> FSPChannel
        -> StatusChan
        -> MgrChannel -> Int
        -> IO ThreadId
connect (host, port, pid, ih, pm) pool pieceMgrC fsC statC mgrC nPieces =
    spawn (connector >> return ())
  where connector =
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
              Right (_caps, _rpid) ->
                  do debugM "Process.PeerMgr.connect" "entering peerP loop code"
                     supC <- channel -- TODO: Should be linked later on
                     children <- peerChildren h mgrC pieceMgrC fsC statC pm nPieces
                     sync $ transmit pool $ SpawnNew (Supervisor $ allForOne "PeerSup" children)
                     return ()

acceptor :: (Handle, HostName, PortNumber) -> InfoHash -> (InfoHash -> Bool) -> SupervisorChan
         -> PeerId -> MgrChannel -> PieceMgrChannel -> FSPChannel -> StatusChan
         -> PieceMap -> Int
         -> IO ThreadId
acceptor (h,hn,pn) ih ihTst pool pid mgrC pieceMgrC fsC statC pm nPieces =
    spawn (connector >> return ())
  where connector = do
            debugLog "Handling incoming connection"
            r <- receiveHandshake h pid ihTst ih
            debugLog "RecvHandshake run"
            case r of
                Left err -> do debugLog ("Incoming Peer handshake failure with " ++ show hn ++ "("
                                            ++ show pn ++ "), error: " ++ err)
                               return ()
                Right (_caps, _rpid) ->
                    do debugLog "entering peerP loop code"
                       supC <- channel -- TODO: Should be linked later on
                       children <- peerChildren h mgrC pieceMgrC fsC statC pm nPieces
                       sync $ transmit pool $ SpawnNew (Supervisor $ allForOne "PeerSup" children)
                       return ()
        debugLog = debugM "Process.PeerMgr.acceptor"

showPort :: PortID -> String
showPort (PortNumber pn) = show pn
showPort _               = "N/A"
