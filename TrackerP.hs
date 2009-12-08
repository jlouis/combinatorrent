module TrackerP

where

import Control.Concurrent.CHP
import Control.Monad()
import Control.Monad.Trans()

import Data.Char (ord)
import Data.List (intersperse)
import Numeric (showHex)

import qualified Status
data Tasks = NeedPeers -- Don't fret too much, just ask the tracker

-- Should be in another place
data Peer = MkPeer { peerIP :: String,
                     peerPort :: Int }

data TrackerState = Started | Stopped | Completed

instance Show TrackerState where
    show Started = "started"
    show Stopped = "stopped"
    show Completed = "completed"

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


buildRequestUrl :: State -> String
buildRequestUrl s = concat [announceUrl s, "?", concat hlist]
    where hlist :: [String]
          hlist = intersperse "&" $ map (\(k,v) -> k ++ "=" ++ v) headers
          headers :: [(String, String)]
          headers = [("info_hash", rfc1738_encode $ infoHash s),
                     ("peer_id", rfc1738_encode $ peerId s),
                     ("uploaded", show $ uploaded s),
                     ("downloaded", show $ downloaded s),
                     ("left", show $ left s),
                     ("port", show $ localPort s),
                     ("compact", "1"),
                     ("event", show $ state s)]

-- Carry out Url-encoding of a string
rfc1738_encode :: String -> String
rfc1738_encode str = concatMap (\c -> if unreserved c then show c else encode c) str
    where unreserved c = c `elem` chars
          -- I killed ~ from this list as the Mainline client doesn't announce it - jlouis
          chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_./"
          encode :: Char -> String
          encode c = "%" ++ (showHex . ord $ c) ""
