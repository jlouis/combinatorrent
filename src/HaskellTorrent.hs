module Main (main)
where

import Control.Concurrent
import Control.Concurrent.CML.Strict
import Control.Monad

import qualified Data.ByteString as B
import Data.List

import System.Environment
import System.Random

import System.Console.GetOpt
import System.Directory (doesDirectoryExist)
import System.FilePath ()
import System.Log.Logger
import System.Log.Handler.Simple
import System.IO as SIO

import qualified Protocol.BCode as BCode
import qualified Process.Console as Console
import qualified Process.FS as FSP
import qualified Process.PeerMgr as PeerMgr
import qualified Process.PieceMgr as PieceMgr (start, createPieceDb, ChokeInfoChannel)
import qualified Process.ChokeMgr as ChokeMgr (start)
import qualified Process.Status as Status
import qualified Process.Tracker as Tracker
import qualified Process.Listen as Listen
import qualified Process.DirWatcher as DirWatcher (start, DirWatchChan)
import qualified Process.TorrentManager as TorrentManager (start)
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

data Flag = Version | Debug | LogFile FilePath | WatchDir FilePath
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['V','?']        ["version"] (NoArg Version)         "Show version number"
  , Option ['D']            ["debug"]   (NoArg Debug)           "Spew extra debug information"
  , Option []               ["logfile"] (ReqArg LogFile "FILE") "Choose a filepath on which to log"
  , Option ['W']            ["watchdir"] (ReqArg WatchDir "DIR") "Choose a directory to watch for torrents"
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
progHeader = putStrLn $ "This is Haskell-torrent version " ++ version ++ "\n" ++
                        "  For help type 'help'\n"

setupLogging :: [Flag] -> IO ()
setupLogging flags = do
    rootL <- getRootLogger
    fLog <- case logFlag flags of
                Nothing -> streamHandler SIO.stdout DEBUG
                Just (LogFile fp) -> fileHandler fp DEBUG
    when (Debug `elem` flags)
          (updateGlobalLogger rootLoggerName
                 (setHandlers [fLog] . (setLevel DEBUG)))
  where logFlag = find (\e -> case e of
                                LogFile _ -> True
                                _         -> False)

setupDirWatching :: [Flag] -> DirWatcher.DirWatchChan -> IO [Child]
setupDirWatching flags watchC = do
    case dirWatchFlag flags of
        Nothing -> return []
        Just (WatchDir dir) -> do
            ex <- doesDirectoryExist dir
            if ex
                then do return [ Worker $ DirWatcher.start dir watchC ]
                else do putStrLn $ "Directory does not exist, not watching"
                        return []
  where dirWatchFlag = find (\e -> case e of
                                    WatchDir _ -> True
                                    _          -> False)

download :: [Flag] -> String -> IO ()
download flags name = do
    torrent <- B.readFile name
    let bcoded = BCode.decode torrent
    case bcoded of
      Left pe -> print pe
      Right bc -> do
           setupLogging flags
           watchC <- channel
           workersWatch <- setupDirWatching flags watchC
           debugM "Main" (show bc)
           -- setup channels
           statusC  <- channel
           waitC    <- channel
           supC <- channel
           pmC <- channel
           chokeC <- channel
           chokeInfoC <- channel
           torrentManagerC <- channel
           debugM "Main" "Created channels"
           -- setup StdGen and Peer data
           gen <- getStdGen
           -- Create main supervisor process
           let pid = mkPeerId gen
           tid <- allForOne "MainSup"
                     (workersWatch ++
                     [ Worker $ Console.start waitC statusC
                     , Worker $ TorrentManager.start watchC chokeInfoC statusC pid pmC
                     , Worker $ PeerMgr.start pmC pid
                                    chokeC torrentManagerC
                     , Worker $ ChokeMgr.start chokeC chokeInfoC 100 -- 100 is upload rate in KB
                                    False -- TODO: Fix this leeching/seeding problem
                     , Worker $ Listen.start defaultPort pmC
                     ]) supC
           startTorrent bc chokeInfoC statusC pid pmC torrentManagerC
           sync $ receive waitC (const True)
           infoM "Main" "Closing down, giving processes 10 seconds to cool off"
           sync $ transmit supC (PleaseDie tid)
           threadDelay $ 10*1000000
           infoM "Main" "Done..."
           return ()

startTorrent :: BCode.BCode
                -> PieceMgr.ChokeInfoChannel
                -> Channel Status.ST
                -> PeerId
                -> PeerMgr.PeerMgrChannel
                -> Channel PeerMgr.ManageMsg
                -> IO ThreadId
startTorrent bc chokeInfoC statusC pid pmC torrentManagerC = do
           fspC <- channel
           trackerC   <- channel
           supC       <- channel
           chokeInfoC <- channel
           statInC <- channel
           pieceMgrC <- channel
           (handles, haveMap, pieceMap) <- openAndCheckFile bc
           let left = bytesLeft haveMap pieceMap
               clientState = determineState haveMap
           ti <- mkTorrentInfo bc
           tid <- allForOne "TorrentSup"
                     [ Worker $ FSP.start handles pieceMap fspC
                     , Worker $ PieceMgr.start pieceMgrC fspC chokeInfoC statInC
                                        (PieceMgr.createPieceDb haveMap pieceMap)
                     , Worker $ Status.start left clientState statusC statInC trackerC
                     , Worker $ Tracker.start (infoHash ti) ti pid defaultPort statusC statInC
                                        trackerC pmC
                     ] supC
           sync $ transmit torrentManagerC $ PeerMgr.NewTorrent (infoHash ti)
                            (PeerMgr.TorrentLocal pieceMgrC fspC statInC pieceMap )
           sync $ transmit trackerC Status.Start
           return tid
