module Main (main)
where

import Control.Concurrent.CML

import qualified Data.ByteString as B

import System.Environment
import System.Random


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

main :: IO ()
main = getArgs >>= run


run :: [String] -> IO ()
run args = do
    putStrLn $ "This is Haskell-torrent version " ++ version
    case args of
        []       -> putStrLn "*** Usage: haskellTorrent <file.torrent>"
        (name:_) -> download name

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
