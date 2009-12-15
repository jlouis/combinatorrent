module Main (main)
where

import Control.Concurrent.CML
import Data.Maybe
import System.Environment
import System.Random

import Torrent

import qualified BCode
import qualified TrackerP
import qualified StatusP
import qualified ConsoleP
import qualified PeerMgrP()
import qualified TimerP()

main :: IO ()
main = do
  [fname] <- getArgs
  torrent <- readFile fname
  let bcoded = BCode.decode torrent
  case bcoded of
    Left pe -> print pe
    Right bc -> do trackerC <- channel
                   statusC  <- channel
                   waitCh <- channel
                   logC <- ConsoleP.start waitCh
                   ciC  <- channel
                   pmC <- channel
                   gen <- getStdGen
                   let pid = mkPeerId gen
                   let ti = fromJust $ mkTorrentInfo bc
                   StatusP.start logC 10000 StatusP.Leeching statusC ciC
                   TrackerP.start ti pid haskellTorrentPort logC statusC ciC trackerC pmC
                   sync $ receive waitCh (const True)
                   return ()

