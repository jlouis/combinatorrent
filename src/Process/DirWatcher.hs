-- | The DirWatcher Process runs a watcher over a directory. It will tell about any change
--   happening inside that directory.
module Process.DirWatcher (
    -- * Types
      DirWatchMsg(..)
    -- * Channels
    , DirWatchChan
    -- * Interface
    , start
    )
where

import Control.Concurrent
import Control.Concurrent.CML.Strict
import Control.DeepSeq

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Set as S

import System.Directory
import System.FilePath

import Prelude hiding (log)
import Process
import Supervisor

data DirWatchMsg = AddedTorrent FilePath
                 | RemovedTorrent FilePath

instance NFData DirWatchMsg where
  rnf a = a `seq` ()

type DirWatchChan = Channel [DirWatchMsg]

data CF = CF { reportCh :: DirWatchChan
             , dirToWatch :: FilePath }

type ST = S.Set FilePath

instance Logging CF where
    logName _ = "Process.DirWatcher"

start :: FilePath -- ^ Path to watch
      -> DirWatchChan -- ^ Channel to return answers on
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
    syncP =<< sendPC reportCh (map AddedTorrent added ++ map RemovedTorrent removed)
    -- Make ready for next iteration
    put torrents

