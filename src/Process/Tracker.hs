-- | The TrackerP module is responsible for keeping in touch with the Tracker
-- of a torrent.  The tracker is contacted periodically, and we exchange
-- information with it. Specifically, we tell the tracker how much we have
-- downloaded, uploaded and what is left. We also tell it about our current
-- state (i.e., are we a seeder or a leecher?).
--
-- The tracker responds to us with a new set of Peers and general information
-- about the torrent in question. It may also respond with an error in which
-- case we should present it to the user.
--
{-# LANGUAGE CPP #-}
module Process.Tracker
    ( start
    )
where

#if __GLASGOW_HASKELL__ <= 708
import AdaptGhcVersion
#endif

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State hiding (state)

import Data.Bits
import Data.Char (chr, ord)
import Data.List (partition)
import Numeric (showHex)
import qualified Data.ByteString as B
import Data.Word

import Network.Socket as S
import Network.HTTP hiding (port, urlEncode, urlEncodeVars)
import Network.URI hiding (unreserved, reserved)
import Network.Stream

import Control.Exception

import Protocol.BCode as BCode hiding (encode, announceList)
import Process
import Channels
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
        statusPCh :: Status.StatusChannel
      , trackerMsgCh :: TrackerChannel
      , peerMgrCh :: PeerMgr.PeerMgrChannel
      , cfInfoHash :: InfoHash
      }

instance Logging CF where
    logName _ = "Process.Tracker"

-- | Internal state of the tracker CHP process
data ST = ST {
        torrentInfo :: TorrentInfo
      , announceList :: [[AnnounceURL]]  -- will store it separate from TorrentInfo as it could be updated
      , peerId :: PeerId
      , state :: TrackerEvent
      , localPort :: Word16
      , nextTick :: Integer
      }

start :: InfoHash -> TorrentInfo -> PeerId -> Word16
      -> Status.StatusChannel -> TrackerChannel -> PeerMgr.PeerMgrChannel
      -> SupervisorChannel -> IO ThreadId
start ih ti pid port statusC msgC pc supC =
       spawnP (CF statusC msgC pc ih) (ST ti (announceURLs ti) pid Stopped port 0)
                    ({-# SCC "Tracker" #-} cleanupP loop
                        (defaultStopHandler supC)
                        stopEvent)
  where
    stopEvent = modify (\s -> s { state = Stopped }) >> talkTracker
    loop = do
          ch <- asks trackerMsgCh
          msg <- liftIO . atomically $ readTChan ch
          debugP $ "Got tracker event"
          case msg of
            TrackerTick x ->
                do t <- gets nextTick
                   when (x+1 == t) talkTracker
            Stop     ->
                modify (\s -> s { state = Stopped }) >> talkTracker
            Start    ->
                modify (\s -> s { state = Started }) >> talkTracker
            Complete ->
                modify (\s -> s { state = Completed }) >> talkTracker
          loop
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
    v <- liftIO $ newEmptyTMVarIO
    ih <- asks cfInfoHash
    asks statusPCh >>=
        (\ch -> liftIO . atomically $ writeTChan ch (Status.RequestStatus ih v))
    upDownLeft <- liftIO . atomically $ takeTMVar v
    resp <- queryTrackers upDownLeft
    case resp of
        Left err -> do infoP $ "Tracker HTTP Error: " ++ err
                       return (failTimerInterval, Just failTimerInterval)
        Right (ResponseWarning wrn) ->
                    do infoP $ "Tracker Warning Response: " ++ fromBS wrn
                       return (failTimerInterval, Just failTimerInterval)
        Right (ResponseError err) ->
                    do infoP $ "Tracker Error Response: " ++ fromBS err
                       return (failTimerInterval, Just failTimerInterval)
        Right (ResponseDecodeError err) ->
                    do infoP $ "Response Decode error: " ++ fromBS err
                       return (failTimerInterval, Just failTimerInterval)
        Right bc -> do
            c <- asks peerMgrCh
            liftIO . atomically $ writeTChan c (PeerMgr.PeersFromTracker ih $ newPeers bc)
            let trackerStats = Status.TrackerStat
                 { Status.trackInfoHash = ih
                 , Status.trackComplete = completeR bc
                 , Status.trackIncomplete = incompleteR bc }
            asks statusPCh >>= \ch -> liftIO . atomically $ writeTChan ch trackerStats
            eventTransition
            return (timeoutInterval bc, timeoutMinInterval bc)

timerUpdate :: (Integer, Maybe Integer) -> Process CF ST ()
timerUpdate (timeout, _minTimeout) = do
    st <- gets state
    when (st == Running)
        (do t <- tick
            ch <- asks trackerMsgCh
            _ <- Timer.registerSTM (fromIntegral timeout) ch (TrackerTick t)
            debugP $ "Set timer to: " ++ show timeout)
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
                                Nothing -> ResponseDecodeError . toBS $
                                    "Could not decode response properly"
                                Just rok -> rok
  where decodeOk =
            ResponseOk <$> (decodeIps <$> BCode.trackerPeers d)
                       <*> (pure $ BCode.trackerComplete d)
                       <*> (pure $ BCode.trackerIncomplete d)
                       <*> (BCode.trackerInterval d)
                       <*> (pure $ BCode.trackerMinInterval d)


decodeIps :: (B.ByteString, B.ByteString) -> [PeerMgr.Peer]
decodeIps (ipv4, ipv6) = decodeIps4 ipv4 ++ decodeIps6 ipv6

decodeIps4 :: B.ByteString -> [PeerMgr.Peer]
decodeIps4 bs | B.null bs = []
              | B.length bs >= 6 =
                    let (ip, r1) = B.splitAt 4 bs
                        (port, r2) = B.splitAt 2 r1
                        i' = cW32 ip
                        p' = fromIntegral $ cW16 port
                    in PeerMgr.Peer (S.SockAddrInet p' i') : decodeIps4 r2
              | otherwise = [] -- Some trackers fail spectacularly

decodeIps6 :: B.ByteString -> [PeerMgr.Peer]
decodeIps6 bs | B.null bs = []
              | B.length bs >= 18 =
                    let (ip6, r1) = B.splitAt 16 bs
                        (port, r2) = B.splitAt 2 r1
                        i' = cW128 ip6
                        p' = fromIntegral $ cW16 port
                    in PeerMgr.Peer (S.SockAddrInet6 p' 0 i' 0) : decodeIps6 r2
              | otherwise = [] -- Some trackers fail spectacularly

cW32 :: B.ByteString -> Word32
cW32 bs = fromIntegral . sum $ s
  where up = B.unpack bs
        s  = [ fromIntegral b `shiftL` sa | (b, sa) <- zip up [0,8,16,24]] :: [Word32]

cW16 :: B.ByteString -> Word16
cW16 bs = fromIntegral . sum $ s
  where s = [ fromIntegral b `shiftL` sa | (b, sa) <- zip (B.unpack bs) [0,8]] :: [Word16]

cW128 :: B.ByteString -> (Word32, Word32, Word32, Word32)
cW128 bs =
    let (q1, r1) = B.splitAt 4 bs
        (q2, r2) = B.splitAt 4 r1
        (q3, q4) = B.splitAt 4 r2
    in (cW32 q1, cW32 q2, cW32 q3, cW32 q4)



bubbleUpURL :: AnnounceURL -> [AnnounceURL] -> Process CF ST ()
bubbleUpURL _ (_:[]) = return ()
bubbleUpURL _ [] = return ()
bubbleUpURL url tier@(x:_) = if url == x
                              then return ()
                              else do
                                alist <- gets announceList
                                let newTier = url : filter (/=url) tier
                                    newAnnounceList = map (\a -> if a /= tier then a else newTier) alist
                                _ <- modify (\s -> s { announceList = newAnnounceList })
                                return ()

tryThisTier' :: Status.StatusState -> [AnnounceURL] -> Process CF ST (Either String (AnnounceURL, TrackerResponse))
tryThisTier' _ []     = return $ Left "Empty announce-list"
tryThisTier' s (x:xs) = do url <- buildRequestURL s x
                           uri <- case parseURI url of
                             Nothing -> fail $ "Could not parse the url " ++ url
                             Just u  -> return u
                           resp <- trackerRequest uri
                           case resp of
                             Left m  -> if null xs
                                        then return $ Left m
                                        else tryThisTier' s xs
                             Right r -> return $ Right (x, r)


-- | from BEP0012: try first element in list, if it's can't be reached, then second, and so on
--   first successfull URL will bubble up and become the new head of this tier
--   announceList stored in State should be updated to reflect the changes
tryThisTier :: Status.StatusState -> [AnnounceURL] -> Process CF ST (Either String TrackerResponse)
tryThisTier params tier = do resp <- tryThisTier' params tier
                             case resp of
                               Left m -> return $ Left m
                               Right (url, r) -> do bubbleUpURL url tier
                                                    return $ Right r


queryTrackers' :: Status.StatusState -> [[AnnounceURL]] -> Process CF ST (Either String TrackerResponse)
queryTrackers' _ []     = return $ Left "Empty announce-list"
queryTrackers' p (x:[]) = tryThisTier p x --last element, so return whatever it gives us
queryTrackers' p (x:xs) = do resp <- tryThisTier p x
                             case resp of
                               Left  _ -> queryTrackers' p xs -- in case of error, move to the next tier
                               Right _ -> return $ resp       -- if success just return result

queryTrackers :: Status.StatusState ->  Process CF ST (Either String TrackerResponse)
queryTrackers ss = do alist <- gets announceList
                      queryTrackers' ss alist



-- TODO: Do not recurse infinitely here.
trackerRequest :: URI -> Process CF ST (Either String TrackerResponse)
trackerRequest uri =
    do debugP $ "Querying URI: " ++ (show uri)
       resp <- liftIO $ catch (simpleHTTP request) (\e -> let err = show (e :: IOException)
                                                          in return . Left . ErrorMisc $ err)
       case resp of
         Left x -> do let err = "Error connecting: " ++ show x
                      debugP err
                      return $ Left err
         Right r ->
             case rspCode r of
               (2,_,_) ->
                   case BCode.decode . toBS . rspBody $ r of
                     Left pe -> return . Left . show $ pe
                     Right bc -> do debugP $ "Response: " ++ BCode.prettyPrint bc
                                    return $ Right $ processResultDict bc
               (3,_,_) ->
                   case findHeader HdrLocation r of
                     Nothing -> return $ Left (show r)
                     Just newURL -> case parseURI newURL of
                                        Nothing -> return $ Left (show newURL)
                                        Just u -> trackerRequest u
               _ -> return $ Left (show r)
  where request = Request {rqURI = uri,
                           rqMethod = GET,
                           rqHeaders = [],
                           rqBody = ""}

--- Construct a new request URL. Perhaps this ought to be done with the HTTP
--- client library
buildRequestURL :: Status.StatusState -> AnnounceURL -> Process CF ST String
buildRequestURL ss url = do params <- urlEncodeVars <$> buildRequestParams ss
                            let announceString = fromBS url
                             -- announce string might already have some
                             -- parameters in it
                                sep = if '?' `elem` announceString
                                      then "&"
                                      else "?"
                            return $ concat [announceString, sep, params]


-- copied from: the old HTTP library. this commit breaks it for us: b0177fa952a8d0f77e732cb817197864679b1c83
urlEncode :: String -> String
urlEncode [] = []
urlEncode (h:t) = let str = if reserved (ord h) then escape h else [h]
                  in str ++ urlEncode t
 where reserved x
          | x >= ord 'a' && x <= ord 'z' = False
          | x >= ord 'A' && x <= ord 'Z' = False
          | x >= ord '0' && x <= ord '9' = False
          | x <= 0x20 || x >= 0x7F = True
          | otherwise = x `elem` map ord
               [';','/','?',':','@','&'
                ,'=','+',',','$','{','}'
                ,'|','\\','^','[',']','`'
                ,'<','>','#','%','"']

       escape x = '%':showHex (ord x) ""

urlEncodeVars :: [(String,String)] -> String
urlEncodeVars ((n,v):t) =
    let (same,diff) = partition ((==n) . fst) t
    in urlEncode n ++ '=' : foldl (\x y -> x ++ ',' : urlEncode y) (urlEncode $ v) (map snd same)
       ++ urlEncodeRest diff
       where urlEncodeRest [] = []
             urlEncodeRest diff = '&' : urlEncodeVars diff
urlEncodeVars [] = []

buildRequestParams :: Status.StatusState -> Process CF ST [(String, String)]
buildRequestParams ss = do
    s <- get
    p <- gets localPort
    return $
      [("info_hash", map (chr . fromIntegral) . B.unpack . infoHash . torrentInfo $ s),
       ("peer_id", peerId s),
       ("uploaded", show $ Status.uploaded ss),
       ("downloaded", show $ Status.downloaded ss),
       ("left", show $ Status.left ss),
       ("port", show p),
       ("compact", "1")] ++
       (trackerfyEvent $ state s)

trackerfyEvent :: TrackerEvent -> [(String, String)]
trackerfyEvent ev =
    case ev of
        Running   -> []
        Completed -> [("event", "completed")]
        Started   -> [("event", "started")]
        Stopped   -> [("event", "stopped")]
