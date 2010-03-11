module Main (main)
where

import Control.Concurrent
import Control.Concurrent.CML
import Control.Monad

import qualified Data.ByteString as B

import System.Environment
import System.Random

import System.Console.GetOpt
import System.Log.Logger
import System.Log.Handler.Simple
import System.IO as SIO

import qualified Protocol.BCode as BCode
import qualified Process.Console as Console
import qualified Process.FS as FSP
import qualified Process.PeerMgr as PeerMgr
import qualified Process.PieceMgr as PieceMgr (start, createPieceDb)
import qualified Process.ChokeMgr as ChokeMgr (start)
import qualified Process.Status as Status
import qualified Process.Tracker as Tracker
import qualified Process.Listen as Listen
import FS
import Supervisor
import Torrent
import Version
import qualified Test

main :: IO ()
main = do args <- getArgs
          if "--tests" `elem` args
              then Test.runTests
              else progOpts args >>= run

-- COMMAND LINE PARSING

data Flag = Version | Debug
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['V','?']        ["version"] (NoArg Version)         "show version number"
  , Option ['D']            ["debug"]   (NoArg Debug)           "spew extra debug information"
  ]

progOpts :: [String] -> IO ([Flag], [String])
progOpts args = do
    case getOpt Permute options args of
        (o,n,[]  ) -> return (o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: HaskellTorrent [OPTION...] file"

run :: ([Flag], [String]) -> IO ()
run (flags, files) = do
    if Version `elem` flags
        then progHeader
        else case files of
                [] -> putStrLn "No torrentfile input"
                [name] -> progHeader >> download flags name
                _  -> putStrLn "More than one torrent file given"

progHeader :: IO ()
progHeader = putStrLn $ "This is Haskell-torrent version " ++ version

download :: [Flag] -> String -> IO ()
download flags name = do
    torrent <- B.readFile name
    let bcoded = BCode.decode torrent
    case bcoded of
      Left pe -> print pe
      Right bc -> do
           rootL <- getRootLogger
           fLog <- streamHandler SIO.stdout DEBUG
           when (Debug `elem` flags)
                (updateGlobalLogger rootLoggerName
                    (setHandlers [fLog] . (setLevel DEBUG)))
           debugM "Main" (show bc)
           (handles, haveMap, pieceMap) <- openAndCheckFile bc
           -- setup channels
           trackerC <- channel
           statusC  <- channel
           waitC    <- channel
           pieceMgrC <- channel
           supC <- channel
           fspC <- channel
           statInC <- channel
           pmC <- channel
           chokeC <- channel
           chokeInfoC <- channel
           debugM "Main" "Created channels"
           -- setup StdGen and Peer data
           gen <- getStdGen
           ti <- mkTorrentInfo bc
           let pid = mkPeerId gen
               left = bytesLeft haveMap pieceMap
               clientState = determineState haveMap
           -- Create main supervisor process
           tid <- allForOne "MainSup"
                     [ Worker $ Console.start waitC
                     , Worker $ FSP.start handles pieceMap fspC
                     , Worker $ PeerMgr.start pmC pid (infoHash ti)
                                    pieceMap pieceMgrC fspC chokeC statInC (pieceCount ti)
                     , Worker $ PieceMgr.start pieceMgrC fspC chokeInfoC statInC
                                        (PieceMgr.createPieceDb haveMap pieceMap)
                     , Worker $ Status.start left clientState statusC statInC trackerC
                     , Worker $ Tracker.start ti pid defaultPort statusC statInC
                                        trackerC pmC
                     , Worker $ ChokeMgr.start chokeC chokeInfoC 100 -- 100 is upload rate in KB
                                    (case clientState of
                                        Seeding -> True
                                        Leeching -> False)
                     , Worker $ Listen.start defaultPort pmC
                     ] supC
           sync $ transmit trackerC Status.Start
           sync $ receive waitC (const True)
           infoM "Main" "Closing down, giving processes 10 seconds to cool off"
           sync $ transmit supC (PleaseDie tid)
           threadDelay $ 10*1000000
           infoM "Main" "Done..."
           return ()
