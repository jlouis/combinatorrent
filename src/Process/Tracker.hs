-- | The TrackerP module is responsible for keeping in touch with the Tracker of a torrent.
--   The tracker is contacted periodically, and we exchange information with it. Specifically,
--   we tell the tracker how much we have downloaded, uploaded and what is left. We also
--   tell it about our current state (i.e., are we a seeder or a leecher?).
--
--   The tracker responds to us with a new set of Peers and general information about the
--   torrent in question. It may also respond with an error in which case we should present
--   it to the user.
--
module Process.Tracker
    ( start
    )
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.CML
import Control.Monad.Reader
import Control.Monad.State

import Data.Char (ord)
import Data.List (intersperse)
import Data.Time.Clock.POSIX
import qualified Data.ByteString as B

import Network
import Network.HTTP hiding (port)
import Network.URI hiding (unreserved)

import Numeric (showHex)


import Protocol.BCode as BCode hiding (encode)
import Logging
import Process
import Supervisor
import Torrent
import qualified Process.Status as Status
import qualified Process.PeerMgr as PeerMgr
import qualified Process.Timer as Timer



-- | The tracker state is used to tell the tracker our current state. In order
--   to output it correctly, we override the default show instance with the
--   version below. This may be wrong to do in the long run, but for now it works
--   fine.
--
--   The state is either started or stopped upon the client starting. The
--   tracker will create an entry for us if we tell it that we started, and it
--   will tear down this entry if we tell it that we stopped. It will know if
--   we are a seeder or a leecher based on how much data is left for us to
--   download.
--
--   the 'Completed' entry is used once in the lifetime of a torrent. It
--   explains to the tracker that we completed the torrent in question.
data TrackerEvent = Started | Stopped | Completed | Running
    deriving Eq



-- | The tracker will in general respond with a BCoded dictionary. In our world, this is
--   not the data structure we would like to work with. Hence, we parse the structure into
--   the ADT below.
data TrackerResponse = ResponseOk { newPeers :: [PeerMgr.Peer],
                                    completeR :: Maybe Integer,
                                    incompleteR :: Maybe Integer,
                                    timeoutInterval :: Integer,
                                    timeoutMinInterval :: Maybe Integer }
                     | ResponseDecodeError B.ByteString
                     | ResponseWarning B.ByteString
                     | ResponseError B.ByteString

-- | If we fail to contact the tracker, we will wait for 15 minutes. The number is quite arbitrarily chosen
failTimerInterval :: Integer
failTimerInterval = 15 * 60

-- | Configuration of the tracker process
data CF = CF {
        logCh :: LogChannel
      , statusCh :: Channel Status.ST
      , statusPCh :: Channel Status.StatusMsg
      , trackerMsgCh :: Channel Status.TrackerMsg
      , peerMgrCh :: PeerMgr.PeerMgrChannel
      }

instance Logging CF where
  getLogger cf = ("TrackerP", logCh cf)

-- | Internal state of the tracker CHP process
data ST = ST {
        torrentInfo :: TorrentInfo
      , peerId :: PeerId
      , state :: TrackerEvent
      , localPort :: PortID
      , nextContactTime :: POSIXTime
      , nextTick :: Integer
      }

start :: TorrentInfo -> PeerId -> PortID -> LogChannel -> Channel Status.ST
      -> Channel Status.StatusMsg -> Channel Status.TrackerMsg -> PeerMgr.PeerMgrChannel
      -> SupervisorChan -> IO ThreadId
start ti pid port logC sc statusC msgC pc supC =
    do tm <- getPOSIXTime
       spawnP (CF logC sc statusC msgC pc) (ST ti pid Stopped port tm 0)
                    (cleanupP (forever loop)
                        (defaultStopHandler supC)
                        stopEvent)
  where
    stopEvent :: Process CF ST ()
    stopEvent = do
        logDebug "Stopping... telling tracker"
        modify (\s -> s { state = Stopped }) >> talkTracker
    loop :: Process CF ST ()
    loop = do
          msg <- recvPC trackerMsgCh >>= syncP
          logDebug $ "Got tracker event"
          case msg of
            Status.TrackerTick x ->
                do t <- gets nextTick
                   when (x+1 == t) talkTracker
            Status.Stop     ->
                modify (\s -> s { state = Stopped }) >> talkTracker
            Status.Start    ->
                modify (\s -> s { state = Started }) >> talkTracker
            Status.Complete ->
                  modify (\s -> s { state = Completed }) >> talkTracker
    talkTracker = pokeTracker >>= timerUpdate

eventTransition :: Process CF ST ()
eventTransition = do
    st <- gets state
    modify (\s -> s { state = newS st})
  where newS st =
         case st of
            Running -> Running
            Stopped -> Stopped
            Completed -> Running
            Started -> Running

-- | Poke the tracker. It returns the new timer intervals to use
pokeTracker :: Process CF ST (Integer, Maybe Integer)
pokeTracker = do
    upDownLeft <- syncP =<< recvPC statusCh
    url <- buildRequestURL upDownLeft
    logDebug $ "Request URL: " ++ url
    uri <- case parseURI url of
            Nothing -> fail $ "Could not parse the url " ++ url
            Just u  -> return u
    resp <- trackerRequest uri
    case resp of
        Left err -> do logInfo $ "Tracker HTTP Error: " ++ err
                       return (failTimerInterval, Just failTimerInterval)
        Right (ResponseWarning wrn) ->
                    do logInfo $ "Tracker Warning Response: " ++ fromBS wrn
                       return (failTimerInterval, Just failTimerInterval)
        Right (ResponseError err) ->
                    do logInfo $ "Tracker Error Response: " ++ fromBS err
                       return (failTimerInterval, Just failTimerInterval)
        Right (ResponseDecodeError err) ->
                    do logInfo $ "Response Decode error: " ++ fromBS err
                       return (failTimerInterval, Just failTimerInterval)
        Right bc -> do sendPC peerMgrCh (PeerMgr.PeersFromTracker $ newPeers bc) >>= syncP
                       let trackerStats = Status.TrackerStat { Status.trackComplete = completeR bc,
                                                               Status.trackIncomplete = incompleteR bc }
                       sendPC statusPCh trackerStats  >>= syncP
                       eventTransition
                       return (timeoutInterval bc, timeoutMinInterval bc)

timerUpdate :: (Integer, Maybe Integer) -> Process CF ST ()
timerUpdate (timeout, minTimeout) = do
    st <- gets state
    when (st == Running)
        (do t <- tick
            ch <- asks trackerMsgCh
            Timer.register timeout (Status.TrackerTick t) ch
            logDebug $ "Set timer to: " ++ show timeout)
  where tick = do t <- gets nextTick
                  modify (\s -> s { nextTick = t + 1 })
                  return t

-- Process a result dict into a tracker response object.
processResultDict :: BCode -> TrackerResponse
processResultDict d =
    case BCode.trackerError d of
      Just err -> ResponseError err
      Nothing -> case BCode.trackerWarning d of
                   Just warn -> ResponseWarning warn
                   Nothing -> case decodeOk of
                                Nothing -> ResponseDecodeError . toBS $ "Could not decode response properly"
                                Just rok -> rok
  where decodeOk =
            ResponseOk <$> (decodeIps <$> BCode.trackerPeers d)
                       <*> (pure $ BCode.trackerComplete d)
                       <*> (pure $ BCode.trackerIncomplete d)
                       <*> (BCode.trackerInterval d)
                       <*> (pure $ BCode.trackerMinInterval d)


decodeIps :: B.ByteString -> [PeerMgr.Peer]
decodeIps = decodeIps' . fromBS

-- Decode a list of IP addresses. We expect these to be a compact response by default.
decodeIps' :: String -> [PeerMgr.Peer]
decodeIps' [] = []
decodeIps' (b1 : b2 : b3 : b4 : p1 : p2 : rest) = PeerMgr.Peer ip port : decodeIps' rest
  where ip = concat . intersperse "." . map (show . ord) $ [b1, b2, b3, b4]
        port = PortNumber . fromIntegral $ ord p1 * 256 + ord p2
decodeIps' xs = error $ "decodeIps': invalid IPs: " ++ xs -- Quench all other cases

trackerRequest :: URI -> Process CF ST (Either String TrackerResponse)
trackerRequest uri =
    do resp <- liftIO $ simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r ->
             case rspCode r of
               (2,_,_) ->
                   case BCode.decode . toBS . rspBody $ r of
                     Left pe -> return $ Left (show pe)
                     Right bc -> do logDebug $ "Response: " ++ BCode.prettyPrint bc
                                    return $ Right $ processResultDict bc
               (3,_,_) ->
                   case findHeader HdrLocation r of
                     Nothing -> return $ Left (show r)
                     Just newURL -> case parseURI newURL of
                                        Nothing -> return $ Left (show newURL)
                                        Just uri -> trackerRequest uri
               _ -> return $ Left (show r)
  where request = Request {rqURI = uri,
                           rqMethod = GET,
                           rqHeaders = [],
                           rqBody = ""}

-- Construct a new request URL. Perhaps this ought to be done with the HTTP client library
buildRequestURL :: Status.ST -> Process CF ST String
buildRequestURL ss = do ti <- gets torrentInfo
                        hdrs <- headers
                        let hl = concat $ hlist hdrs
                        return $ concat [fromBS $ announceURL ti, "?", hl]
    where hlist x = intersperse "&" $ map (\(k,v) -> k ++ "=" ++ v) x
          headers = do
            s <- get
            p <- prt
            return $ [("info_hash", rfc1738Encode $ infoHash $ torrentInfo s),
                      ("peer_id",   rfc1738Encode $ peerId s),
                      ("uploaded", show $ Status.uploaded ss),
                      ("downloaded", show $ Status.downloaded ss),
                      ("left", show $ Status.left ss),
                      ("port", show p),
                      ("compact", "1")] ++
                      (trackerfyEvent $ state s)
          prt :: Process CF ST Integer
          prt = do lp <- gets localPort
                   case lp of
                     PortNumber pnum -> return $ fromIntegral pnum
                     _ -> do fail "Unknown port type"
          trackerfyEvent ev =
                case ev of
                    Running   -> []
                    Completed -> [("event", "completed")]
                    Started   -> [("event", "started")]
                    Stopped   -> [("event", "stopped")]

-- Carry out URL-encoding of a string. Note that the clients seems to do it the wrong way
--   so we explicitly code it up here in the same wrong way, jlouis.
rfc1738Encode :: String -> String
rfc1738Encode = concatMap (\c -> if unreserved c then [c] else encode c)
    where unreserved = (`elem` chars)
          -- I killed ~ from this list as the Mainline client doesn't announce it - jlouis
          chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_./"
          encode :: Char -> String
          encode c = '%' : pHex c
          pHex c =
              let p = (showHex . ord $ c) ""
              in if length p == 1 then '0' : p else p
