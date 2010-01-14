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
import qualified ChokeMgrP ()
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
           putStrLn "Created channels"
	   supC <- channel
	   logC <- channel
	   fspC <- channel
           ciC <- channel
           pmC <- channel
           gen <- getStdGen
           let pid = mkPeerId gen
           let ti = fromJust $ mkTorrentInfo bc
	   -- Create main supervisor process
	   allForOne [Worker $ ConsoleP.start logC waitC,
		      Worker $ FSP.start h logC pieceMap fspC,
		      Worker $ PeerMgrP.start pmC pid (infoHash ti)
				    pieceMap pieceMgrC fspC logC (pieceCount ti),
		      Worker $ PieceMgrP.start logC pieceMgrC fspC
					(PieceMgrP.createPieceDb haveMap pieceMap),
		      Worker $ StatusP.start logC 0 StatusP.Leeching statusC ciC,
		      Worker $ TrackerP.start ti pid defaultPort logC statusC ciC
					trackerC pmC] supC
           sync $ receive waitC (const True)
           TrackerP.poison trackerC -- This is probably wrong.
           return ()
