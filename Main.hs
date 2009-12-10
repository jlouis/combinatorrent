module Main (main)
where

import System.Environment

import qualified BCode
import qualified TrackerP()
import qualified StatusP()
import qualified PeerMgrP()
import qualified TimerP()

main :: IO ()
main = do
  [fname] <- getArgs
  torrent <- readFile fname
  let bcoded = BCode.decode torrent
  case bcoded of
    Left pe -> print pe
    Right bc -> print bc

