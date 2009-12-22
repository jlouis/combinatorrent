module Main (main)
where

import Control.Concurrent.CML
import Control.Monad

import Data.Maybe

import System.Environment
import System.Random

import qualified BCode
import qualified ConsoleP
import FS
import qualified FSP
import qualified PeerMgrP
import qualified StatusP
import qualified TimerP()
import Torrent
import qualified TrackerP

main :: IO ()
main = do
  [fname] <- getArgs
  torrent <- readFile fname
  let bcoded = BCode.decode torrent
  case bcoded of
    Left pe -> print pe
    Right bc -> do (h, missingMap, pieceMap) <- openAndCheckFile bc
                   unless (canSeed missingMap) $ fail "We don't have the full file, we can't seed"
                   trackerC <- channel
                   statusC  <- channel
                   waitCh <- channel
                   putStrLn "Created channels"
                   logC <- ConsoleP.start waitCh
                   putStrLn "Started logger"
                   -- The fst of the following is for writing data
                   (_, fspC) <- FSP.start h logC pieceMap
                   ciC  <- channel
                   pmC <- channel
                   gen <- getStdGen
                   let pid = mkPeerId gen
                   let ti = fromJust $ mkTorrentInfo bc
                   putStrLn $ "Created various data, pieceCount is " ++ show (pieceCount ti)
                   PeerMgrP.start pmC pid (infoHash ti) fspC logC (pieceCount ti)
                   StatusP.start logC 0 StatusP.Leeching statusC ciC -- TODO: Fix the 0 here
                   putStrLn "Started Status Process"
                   TrackerP.start ti pid haskellTorrentPort logC statusC ciC trackerC pmC
                   putStrLn "Started Tracker Process"
                   sync $ receive waitCh (const True)
                   TrackerP.poison trackerC
                   return ()
