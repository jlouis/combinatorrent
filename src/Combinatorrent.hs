module Main (main)
where

import Control.Concurrent
import Control.Concurrent.CML.Strict
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.State

import Data.List

import System.Environment
import System.Random

import System.Console.GetOpt
import System.Directory (doesDirectoryExist)
import System.FilePath ()
import System.Log.Logger
import System.Log.Handler.Simple
import System.IO as SIO

import qualified Process.Console as Console
import qualified Process.PeerMgr as PeerMgr
import qualified Process.ChokeMgr as ChokeMgr (start)
import qualified Process.Listen as Listen
import qualified Process.DirWatcher as DirWatcher (start)
import qualified Process.Status as Status (start, StatusChannel, PStat)
import qualified Process.TorrentManager as TorrentManager (start, TorrentMgrChan, TorrentManagerMsg(..))

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

data Flag = Version | Debug | LogFile FilePath | WatchDir FilePath | StatFile FilePath
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['V','?']        ["version"] (NoArg Version)         "Show version number"
  , Option ['D']            ["debug"]   (NoArg Debug)           "Spew extra debug information"
  , Option []               ["logfile"] (ReqArg LogFile "FILE") "Choose a filepath on which to log"
  , Option ['W']            ["watchdir"] (ReqArg WatchDir "DIR") "Choose a directory to watch for torrents"
  , Option ['S']            ["statfile"] (ReqArg StatFile "FILE") "Choose a file to gather stats into"
  ]

progOpts :: [String] -> IO ([Flag], [String])
progOpts args = do
    case getOpt Permute options args of
        (o,n,[]  ) -> return (o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: Combinatorrent [OPTION...] file"

run :: ([Flag], [String]) -> IO ()
run (flags, files) = do
    if Version `elem` flags
        then progHeader
        else case files of
                [] -> putStrLn "No torrentfile input"
                names -> progHeader >> download flags names

progHeader :: IO ()
progHeader = putStrLn $ "This is Combinatorrent \x2620 version " ++ version ++ "\n" ++
                        "  For help type 'help'\n"

setupLogging :: [Flag] -> IO ()
setupLogging flags = do
    fLog <- case logFlag flags of
                Nothing -> streamHandler SIO.stdout DEBUG
                Just (LogFile fp) -> fileHandler fp DEBUG
                Just _ -> error "Impossible match"
    when (Debug `elem` flags)
          (updateGlobalLogger rootLoggerName
                 (setHandlers [fLog] . (setLevel DEBUG)))
  where logFlag = find (\e -> case e of
                                LogFile _ -> True
                                _         -> False)

setupDirWatching :: [Flag] -> TorrentManager.TorrentMgrChan -> IO [Child]
setupDirWatching flags watchC = do
    case dirWatchFlag flags of
        Nothing -> return []
        Just (WatchDir dir) -> do
            ex <- doesDirectoryExist dir
            if ex
                then do return [ Worker $ DirWatcher.start dir watchC ]
                else do putStrLn $ "Directory does not exist, not watching"
                        return []
        Just _ -> error "Impossible match"
  where dirWatchFlag = find (\e -> case e of
                                    WatchDir _ -> True
                                    _          -> False)

setupStatus :: [Flag] -> Status.StatusChannel -> TVar [Status.PStat] -> Child
setupStatus flags statusC stv =
    case statFileFlag flags of
      Nothing -> Worker $ Status.start Nothing statusC stv
      Just (StatFile fn) -> Worker $ Status.start (Just fn) statusC stv
      Just _ -> error "Impossible match"
  where statFileFlag = find (\e -> case e of
                                    StatFile _ -> True
                                    _          -> False)

generatePeerId :: IO PeerId
generatePeerId = do
    gen <- getStdGen
    return $ mkPeerId gen

download :: [Flag] -> [String] -> IO ()
download flags names = do
    setupLogging flags
    watchC <- channel
    workersWatch <- setupDirWatching flags watchC
    -- setup channels
    statusC  <- channel
    waitC    <- channel
    supC <- channel
    pmC <- liftIO $ newTChanIO
    chokeC <- liftIO $ newTChanIO
    rtv <- atomically $ newTVar []
    stv <- atomically $ newTVar []
    debugM "Main" "Created channels"
    pid <- generatePeerId
    tid <- allForOne "MainSup"
              (workersWatch ++
              [ Worker $ Console.start waitC statusC
              , Worker $ TorrentManager.start watchC statusC stv chokeC pid pmC
              , setupStatus flags statusC stv
              , Worker $ PeerMgr.start pmC pid chokeC rtv
              , Worker $ ChokeMgr.start chokeC rtv 100 -- 100 is upload rate in KB
              , Worker $ Listen.start defaultPort pmC
              ]) supC
    sync $ transmit watchC (map TorrentManager.AddedTorrent names)
    sync $ receive waitC (const True)
    infoM "Main" "Closing down, giving processes 10 seconds to cool off"
    sync $ transmit supC (PleaseDie tid)
    threadDelay $ 10*1000000
    infoM "Main" "Done..."
    return ()

