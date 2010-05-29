-- | The timer module is responsible for timing in the project. With
--   the timer system, we can register timers in the future and then
--   we can get a tick triggered at the point in time where we need to
--   act. This allows us to postpone events into the future at a
--   designated time.
--
module Process.Timer
    ( registerSTM
    , registerMV
    )

where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans

registerSTM :: MonadIO m => Int -> TChan a -> a -> m ThreadId
registerSTM secs c m = liftIO $ forkIO $ {-# SCC "Timer" #-} do
    threadDelay (secs * 1000000)
    atomically $ writeTChan c m

registerMV :: MonadIO m => Int -> Chan a -> a -> m ThreadId
registerMV secs c m = liftIO $ forkIO $ do
    threadDelay (secs * 1000000)
    writeChan c m

