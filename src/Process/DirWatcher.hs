-- | The DirWatcher Process runs a watcher over a directory. It will tell about any change
--   happening inside that directory.
module Process.DirWatcher (
    -- * Interface
    start
    )
where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Set as S

import System.Directory
import System.FilePath

import Prelude hiding (log)
import Process
import Process.TorrentManager hiding (start)
import Supervisor


data CF = CF { reportCh :: TorrentMgrChan -- ^ Channel for reporting directory changes
             , dirToWatch :: FilePath }

type ST = S.Set FilePath

instance Logging CF where
    logName _ = "Process.DirWatcher"

start :: FilePath -- ^ Path to watch
      -> TorrentMgrChan -- ^ Channel to return answers on
      -> SupervisorChannel
      -> IO ThreadId
start fp chan supC = do
    spawnP (CF chan fp) S.empty
            (catchP (forever pgm) (defaultStopHandler supC))
  where pgm = do
        q <- liftIO $ registerDelay (5 * 1000000)
        liftIO . atomically $ do
            b <- readTVar q
            if b then return () else retry
        processDirectory

processDirectory :: Process CF ST ()
processDirectory = do
    watchDir <- asks dirToWatch
    files <- liftIO $ getDirectoryContents watchDir
    let torrents = S.fromList $ filter (\fp -> (== ".torrent") $ snd . splitExtension $ fp) files
    running <- get
    let (added, removed) = (S.toList $ S.difference torrents running,
                            S.toList $ S.difference running torrents)
        msg = (map AddedTorrent added ++ map RemovedTorrent removed)
    when (msg /= [])
        (do rc <- asks reportCh
            liftIO . atomically $ writeTChan rc msg
            -- Make ready for next iteration
            put torrents)

