module Main (main)
where

import Control.Concurrent.CML
import Control.Monad

import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L


import System.Environment
import System.Random


import qualified BCode
import qualified ConsoleP
import FS
import qualified FSP
import qualified PeerMgrP
import qualified PieceMgrP (start, createPieceDb)
import qualified Process ()
import qualified ChokeMgrP (start)
import qualified StatusP
import Supervisor
import qualified TimerP()
import Torrent
import qualified TrackerP

main :: IO ()
main = getArgs >>= run


run :: [String] -> IO ()
run args =
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
        do (h, haveMap, pieceMap) <- openAndCheckFile bc
           -- setup channels
           trackerC <- channel
           statusC  <- channel
           waitC    <- channel
           pieceMgrC <- channel
	   supC <- channel
	   logC <- channel
	   fspC <- channel
           statInC <- channel
           pmC <- channel
	   chokeC <- channel
	   chokeInfoC <- channel
           putStrLn "Created channels"
	   -- setup StdGen and Peer data
           gen <- getStdGen
           let pid = mkPeerId gen
               ti = fromJust $ mkTorrentInfo bc
	       left = bytesLeft haveMap pieceMap
	       clientState = determineState haveMap
	   -- Create main supervisor process
	   allForOne [ Worker $ ConsoleP.start logC waitC
		     , Worker $ FSP.start h logC pieceMap fspC
		     , Worker $ PeerMgrP.start pmC pid (infoHash ti)
				    pieceMap pieceMgrC fspC logC chokeC statInC (pieceCount ti)
		     , Worker $ PieceMgrP.start logC pieceMgrC fspC chokeInfoC statInC
					(PieceMgrP.createPieceDb haveMap pieceMap)
		     , Worker $ StatusP.start logC left clientState statusC statInC
		     , Worker $ TrackerP.start ti pid defaultPort logC statusC statInC
					trackerC pmC
		     , Worker $ ChokeMgrP.start logC chokeC chokeInfoC 100 -- 100 is upload rate in Kilobytes
		     ] supC
           sync $ receive waitC (const True)
           return ()
