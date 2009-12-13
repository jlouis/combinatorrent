module Main (main)
where

import Control.Concurrent.CML
import System.Environment

import qualified BCode
import qualified TrackerP()
import qualified StatusP
import qualified PeerMgrP()
import qualified TimerP()

main :: IO ()
main = do
  [fname] <- getArgs
  torrent <- readFile fname
  let bcoded = BCode.decode torrent
  case bcoded of
    Left pe -> print pe
    Right _bc -> do trackerChannel <- channel
                    statusChannel  <- channel
                    _sp <- StatusP.start 10000 -- Left, just a hack
                                         StatusP.Leeching
                                         trackerChannel
                                         statusChannel
                    return ()

