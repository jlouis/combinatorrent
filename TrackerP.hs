{- The TrackerP module is responsible for keeping in touch with the Tracker of a torrent.
   The tracker is contacted periodically, and we exchange information with it. Specifically,
   we tell the tracker how much we have downloaded, uploaded and what is left. We also
   tell it about our current state (i.e., are we a seeder or a leecher?).

   The tracker responds to us with a new set of Peers and general information about the
   torrent in question. It may also respond with an error in which case we should present
   it to the user.
-}
module TrackerP

where

import Control.Concurrent.CHP
import Control.Monad()
import Control.Monad.Trans()

import Data.Char (ord)
import Data.List (intersperse)
import Data.Maybe (fromJust)
import Numeric (showHex)

import qualified Status
import qualified PeerMgrP
import BCode hiding (encode)


{- The tracker state is used to tell the tracker our current state. In order to output it
   correctly, we override the default show instance with the version below. This may be
   wrong to do in the long run, but for now it works fine.

   The state is either started or stopped upon the client starting. The tracker will create
   an entry for us if we tell it that we started, and it will tear down this entry if we tell
   it that we stopped. It will know if we are a seeder or a leecher based on how much data
   is left for us to download.

   the 'Completed' entry is used once in the lifetime of a torrent. It explains to the
   tracker that we completed the torrent in question.
-}
data TrackerState = Started | Stopped | Completed

instance Show TrackerState where
    show Started = "started"
    show Stopped = "stopped"
    show Completed = "completed"

{- The tracker will in general respond with a BCoded dictionary. In our world, this is
   not the data structure we would like to work with. Hence, we parse the structure into
   the ADT below.
-}
data TrackerResponse = ResponseOk { newPeers :: [PeerMgrP.Peer],
                                    completeR :: Integer,
                                    incompleteR :: Integer,
                                    timeoutInterval :: Integer,
                                    timeoutMinInterval :: Integer }
                     | ResponseWarning String
                     | ResponseError String

-- Internal state of the tracker CHP process
data State = MkState {
      infoHash :: String,
      peerId :: String,
      announceUrl :: String,
      state :: TrackerState,
      uploaded :: Integer,
      downloaded :: Integer,
      left :: Integer,
      localPort :: Integer }

-- Many of the values here should be merged into one
tracker :: String -> String -> String -> Integer -> Integer -> Chanin Status.State -> CHP ()
tracker hash pid url port dleft statusIn = lp $ MkState hash pid url Stopped 0 0 dleft port
  where lp s = updatestatus
          where updatestatus = do st <- readChannel statusIn
                                  let bu = uploaded s + Status.uploaded st
                                  let bd = downloaded s + Status.downloaded st
                                  lp s{uploaded = bu, downloaded = bd}

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

-- TODO
decodeIps :: String -> [PeerMgrP.Peer]
decodeIps _ = []

buildRequestUrl :: State -> String
buildRequestUrl s = concat [announceUrl s, "?", concat hlist]
    where hlist :: [String]
          hlist = intersperse "&" $ map (\(k,v) -> k ++ "=" ++ v) headers
          headers :: [(String, String)]
          headers = [("info_hash", rfc1738Encode $ infoHash s),
                     ("peer_id", rfc1738Encode $ peerId s),
                     ("uploaded", show $ uploaded s),
                     ("downloaded", show $ downloaded s),
                     ("left", show $ left s),
                     ("port", show $ localPort s),
                     ("compact", "1"),
                     ("event", show $ state s)]

-- Carry out Url-encoding of a string
rfc1738Encode :: String -> String
rfc1738Encode = concatMap (\c -> if unreserved c then show c else encode c)
    where unreserved = (`elem` chars)
          -- I killed ~ from this list as the Mainline client doesn't announce it - jlouis
          chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_./"
          encode :: Char -> String
          encode c = "%" : (showHex . ord $ c) ""
