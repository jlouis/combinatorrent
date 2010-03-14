-- | The Manager Process - Manages the torrents and controls them
module Process.TorrentManager (
    -- * Types
    -- * Channels
    -- * Interface
    start
    )
where

import Control.Concurrent

import Control.Monad.Reader

import Prelude hiding (log)
import Process
import Process.DirWatcher (DirWatchChan)

import Supervisor

data CF = CF { tCh :: DirWatchChan }

instance Logging CF where
  logName _ = "Process.TorrentManager"

start :: DirWatchChan -- ^ Channel to watch for changes to torrents
      -> SupervisorChan
      -> IO ThreadId
start chan supC = spawnP (CF chan) () (catchP (forever pgm) (defaultStopHandler supC))
  where pgm = syncP =<< changeEvt
        changeEvt =
            recvWrapPC tCh
                (\ls -> liftIO $ print ls)
