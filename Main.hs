module Main (main)
where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Data.Maybe
import System.Environment
import System.IO.Unsafe
import qualified BCode
import qualified TrackerP

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    [] -> return ()
    m:ms -> do
            putMVar children ms
            takeMVar m
            waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkIO (io `finally` putMVar mvar ())

main :: IO ()
main = do
  [fname] <- getArgs
  torrent <- readFile fname
  let bcoded = BCode.decode torrent
  case bcoded of
    Left pe -> print pe
    Right bc -> do peerC <- newChan
                   taskC <- newChan
                   forkChild  (TrackerP.spawn (fromJust (BCode.announce bc)) peerC taskC)
                   waitForChildren