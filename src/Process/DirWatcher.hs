-- | The DirWatcher Process runs a watcher over a directory. It will tell about any change
--   happening inside that directory.
module Process.DirWatcher (
    -- * Interface
    start
    )
where

import Control.Concurrent

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
      -> SupervisorChan
      -> IO ThreadId
start fp chan supC = do
    spawnP (CF chan fp) S.empty
            (catchP (foreverP pgm) (defaultStopHandler supC))
  where pgm = syncP =<< watchEvt
        watchEvt = do
            ev <- atTimeEvtP 5 ()
            wrapP ev (\_ -> processDirectory)

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
        (do syncP =<< sendPC reportCh msg
            -- Make ready for next iteration
            put torrents)

