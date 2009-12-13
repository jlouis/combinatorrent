-- Haskell Torrent
-- Copyright (c) 2009, Jesper Louis Andersen,
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--  * Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-- | The TrackerP module is responsible for keeping in touch with the Tracker of a torrent.
--   The tracker is contacted periodically, and we exchange information with it. Specifically,
--   we tell the tracker how much we have downloaded, uploaded and what is left. We also
--   tell it about our current state (i.e., are we a seeder or a leecher?).
--
--   The tracker responds to us with a new set of Peers and general information about the
--   torrent in question. It may also respond with an error in which case we should present
--   it to the user.
--
--   TODO List: HTTP Client requests.
--              Timeout handling
module TrackerP
where

import Control.Concurrent.CML

import Data.Char (ord)
import Data.List (intersperse)
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX


import Network.HTTP hiding (port)
import Network.URI hiding (unreserved)

import Numeric (showHex)

import Torrent
import qualified ConsoleP
import qualified PeerMgrP
import qualified StatusP
import qualified TimerP
import BCode hiding (encode)


-- | The tracker state is used to tell the tracker our current state. In order to output it
--   correctly, we override the default show instance with the version below. This may be
--   wrong to do in the long run, but for now it works fine.
--
--   The state is either started or stopped upon the client starting. The tracker will create
--   an entry for us if we tell it that we started, and it will tear down this entry if we tell
--   it that we stopped. It will know if we are a seeder or a leecher based on how much data
--   is left for us to download.
--
--   the 'Completed' entry is used once in the lifetime of a torrent. It explains to the
--   tracker that we completed the torrent in question.
data TrackerState = Started | Stopped | Completed

-- | TrackerChannel is the channel of the tracker
type TrackerChannel = Channel TrackerMsg
data TrackerMsg = Poison | TrackerTick Integer

instance Show TrackerState where
    show Started = "started"
    show Stopped = "stopped"
    show Completed = "completed"

-- | The tracker will in general respond with a BCoded dictionary. In our world, this is
--   not the data structure we would like to work with. Hence, we parse the structure into
--   the ADT below.
data TrackerResponse = ResponseOk { newPeers :: [PeerMgrP.Peer],
                                    completeR :: Integer,
                                    incompleteR :: Integer,
                                    timeoutInterval :: Integer,
                                    timeoutMinInterval :: Integer }
                     | ResponseWarning String
                     | ResponseError String

-- | Internal state of the tracker CHP process
data State = State {
      torrentInfo :: TorrentInfo,
      peerId :: PeerId,
      state :: TrackerState,
      localPort :: Integer,
      logChan :: Channel String,
      statusChanIn :: Channel StatusP.State,
      statusChanOut :: Channel (Integer, Integer),
      nextContactTime :: POSIXTime,
      nextTick :: Integer,
      trackerChan :: TrackerChannel,
      peerChan :: Channel [PeerMgrP.Peer] }

{-
start :: TorrentInfo -> PeerId -> Integer -> Channel String -> StatusP.Chan -> IO ()
start ti pid lp lchan = spawn $ loop s
  where s = State { torrentInfo = ti,
                    peerId = pid,
                    state = Stopped,
                    localPort = lp,
                    logChan = lchan,
                    statusChan = ...
-}

failTimerInterval :: Integer
failTimerInterval = 15 * 60  -- Arbitrarily chosen at 15 minutes

pokeTracker :: State -> IO State
pokeTracker s = do upDownLeft <- sync $ receive (statusChanIn s) (\_ -> True)
                   resp <- trackerRequest (buildRequestUrl s upDownLeft)
                   case resp of
                     Left err -> do ConsoleP.logMsg (logChan s) ("Tracker Error: " ++ err)
                                    timerUpdate s failTimerInterval failTimerInterval
                     Right bc -> do sync $ transmit (peerChan s) (newPeers bc)
                                    sync $ transmit (statusChanOut s) (completeR bc, incompleteR bc)
                                    timerUpdate s (timeoutInterval bc) (timeoutMinInterval bc)

timerUpdate :: State -> Integer -> Integer -> IO State
timerUpdate s interval minInterval =
    do TimerP.register interval (TrackerTick nt) (trackerChan s)
       return $ s {nextTick = nt + 1, nextContactTime = ntime }
  where nt = nextTick s
        ntime = (nextContactTime s) + (fromInteger minInterval)

loop :: State -> IO State
loop s = sync trackerEvent
  where trackerEvent = wrap (receive (trackerChan s) (\_ -> True))
                      (\msg -> case msg of
                                 TrackerTick version -> if version /= nextTick s
                                                        then loop s
                                                        else pokeTracker s >>= loop
                                 Poison -> pokeTracker (s { state = Stopped }))

-- Process a result dict into a tracker response object.
processResultDict :: BCode -> TrackerResponse
processResultDict d =
    case BCode.trackerError d of
      Just err -> ResponseError err
      Nothing -> case BCode.trackerWarning d of
                   Just warn -> ResponseWarning warn
                   Nothing -> decodeOk
  where decodeOk =
            ResponseOk peers complete incomplete interval min_interval
        complete = fromJust $ BCode.trackerComplete d
        incomplete = fromJust $ BCode.trackerIncomplete d
        interval = fromJust $ BCode.trackerInterval d
        min_interval = fromJust $ BCode.trackerMinInterval d
        peers = decodeIps $ fromJust $ BCode.trackerPeers d

-- Decode a list of IP addresses. We expect these to be a compact response by default.
decodeIps :: String -> [PeerMgrP.Peer]
decodeIps [] = []
decodeIps (b1 : b2 : b3 : b4 : p1 : p2 : rest) = PeerMgrP.MkPeer ip port : decodeIps rest
  where ip = (ord b1, ord b2, ord b3, ord b4)
        port = ord p1 * 256 + ord p2
decodeIps _ = undefined -- Quench all other cases

trackerRequest :: String -> IO (Either String TrackerResponse)
trackerRequest url =
    do resp <- simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r ->
             case rspCode r of
               (2,_,_) ->
                   case BCode.decode (rspBody r) of
                     Left pe -> return $ Left (show pe)
                     Right bc -> return $ Right $ processResultDict bc
               (3,_,_) ->
                   case findHeader HdrLocation r of
                     Nothing -> return $ Left (show r)
                     Just newUrl -> trackerRequest newUrl
               _ -> return $ Left (show r)
  where request = Request {rqURI = uri,
                           rqMethod = GET,
                           rqHeaders = [],
                           rqBody = ""}
        uri = fromJust $ parseURI url

-- Construct a new request URL. Perhaps this ought to be done with the HTTP client library
buildRequestUrl :: State -> StatusP.State -> String
buildRequestUrl s ss = concat [announceURL $ torrentInfo s, "?", concat hlist]
    where hlist :: [String]
          hlist = intersperse "&" $ map (\(k,v) -> k ++ "=" ++ v) headers
          headers :: [(String, String)]
          headers = [("info_hash", rfc1738Encode $ infoHash $ torrentInfo s),
                     ("peer_id", rfc1738Encode $ peerId s),
                     ("uploaded", show $ StatusP.uploaded ss),
                     ("downloaded", show $ StatusP.downloaded ss),
                     ("left", show $ StatusP.left ss),
                     ("port", show $ localPort s),
                     ("compact", "1"),
                     ("event", show $ state s)]

-- Carry out Url-encoding of a string. Note that the clients seems to do it the wrong way
--   so we explicitly code it up here in the same wrong way, jlouis.
rfc1738Encode :: String -> String
rfc1738Encode = concatMap (\c -> if unreserved c then show c else encode c)
    where unreserved = (`elem` chars)
          -- I killed ~ from this list as the Mainline client doesn't announce it - jlouis
          chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_./"
          encode :: Char -> String
          encode c = '%' : (showHex . ord $ c) ""
