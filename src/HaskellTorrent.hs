module Main (main)
where

import Control.Concurrent.CML

import qualified Data.ByteString as B

import System.Environment
import System.Random

import System.Console.GetOpt

import qualified Protocol.BCode as BCode
import qualified Process.Console as Console
import qualified Process.FS as FSP
import qualified Process.PeerMgr as PeerMgr
import qualified Process.PieceMgr as PieceMgr (start, createPieceDb)
import qualified Process.ChokeMgr as ChokeMgr (start)
import qualified Process.Status as Status
import qualified Process.Tracker as Tracker
import Logging
import FS
import Supervisor
import Torrent
import Version
import qualified Test

main :: IO ()
main = getArgs >>= progOpts >>= run

-- COMMAND LINE PARSING

data Flag = Test | Version
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['V','?']        ["version"] (NoArg Version)         "show version number"
  , Option ['T']            ["tests"]    (NoArg Test)            "run internal test suite"
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
        else if Test `elem` flags
            then do Test.runTests
            else case files of
                [] -> putStrLn "No torrentfile input"
                [name] -> progHeader >> download name
                _  -> putStrLn "More than one torrent file given"

progHeader :: IO ()
progHeader = putStrLn $ "This is Haskell-torrent version " ++ version

download :: String -> IO ()
download name = do
    torrent <- B.readFile name
    let bcoded = BCode.decode torrent
    case bcoded of
      Left pe -> print pe
      Right bc ->
        do print bc
	   (handles, haveMap, pieceMap) <- openAndCheckFile bc
	   logC <- channel
	   Logging.startLogger logC
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
           putStrLn "Created channels"
	   -- setup StdGen and Peer data
           gen <- getStdGen
	   ti <- mkTorrentInfo bc
           let pid = mkPeerId gen
	       left = bytesLeft haveMap pieceMap
	       clientState = determineState haveMap
	   -- Create main supervisor process
	   allForOne "MainSup"
		     [ Worker $ Console.start logC waitC
		     , Worker $ FSP.start handles logC pieceMap fspC
		     , Worker $ PeerMgr.start pmC pid (infoHash ti)
				    pieceMap pieceMgrC fspC logC chokeC statInC (pieceCount ti)
		     , Worker $ PieceMgr.start logC pieceMgrC fspC chokeInfoC statInC
					(PieceMgr.createPieceDb haveMap pieceMap)
		     , Worker $ Status.start logC left clientState statusC statInC trackerC
		     , Worker $ Tracker.start ti pid defaultPort logC statusC statInC
					trackerC pmC
		     , Worker $ ChokeMgr.start logC chokeC chokeInfoC 100 -- 100 is upload rate in KB
				    (case clientState of
					Seeding -> True
					Leeching -> False)
		     ] logC supC
	   sync $ transmit trackerC Status.Start
           sync $ receive waitC (const True)
           return ()
