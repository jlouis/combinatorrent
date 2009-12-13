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
                   logC <- ConsoleP.start
                   ciC  <- channel
                   pmC <- channel
                   gen <- getStdGen
                   pid <- return $ mkPeerId gen
                   ti <- return $ fromJust $ mkTorrentInfo bc
                   StatusP.start 10000 StatusP.Leeching statusC ciC
                   TrackerP.start ti pid haskellTorrentPort logC statusC ciC trackerC pmC
                   return ()

