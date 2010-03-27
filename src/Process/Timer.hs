-- | The timer module is responsible for timing in the project. With
--   the timer system, we can register timers in the future and then
--   we can get a tick triggered at the point in time where we need to
--   act. This allows us to postpone events into the future at a
--   designated time.
--
module Process.Timer (register)

where

import Control.Concurrent
import Control.Concurrent.CML.Strict
import Control.DeepSeq
import Control.Monad.Trans

-- | Registers a timer tick on a channel in a number of seconds with
--   an annotated version.
registerL :: NFData a => Integer -> a -> Channel a -> IO ()
registerL secs v tickChan = do _ <- spawn timerProcess
                               return ()
  where timerProcess = do threadDelay $ fromInteger $ secs * 1000000
                          sync $ transmit tickChan v


register :: (MonadIO m, NFData a) => Integer -> a -> Channel a -> m ()
register secs v c = liftIO $ registerL secs v c
