-- | Logging primitives
{-# LANGUAGE TypeSynonymInstances #-}
module Logging (
  -- * Classes
    Logging(..)
  -- * Types
  , LogChannel
  , LogPriority(..)
  -- * Interface
  , logInfo
  , logDebug
  , logWarn
  , logFatal
  , logError
  -- * Interface (deprecated)
  , logMsg
  , logMsg'
  )
where

import Control.Concurrent.CML
import Control.Monad.Reader

import Prelude hiding (log)

import Process


data LogPriority = Debug -- ^ Fine grained debug info
		 | Warn  -- ^ Potentially harmful situations
		 | Info  -- ^ Informational messages, progress reports
		 | Error -- ^ Errors that are continuable
		 | Fatal -- ^ Severe errors. Will probably make the application abort
		    deriving (Show, Eq, Ord)

-- | The level by which we log
logLevel :: LogPriority
#ifdef DEBUG
logLevel = Debug
#else
logLevel = Info
#endif

-- | The class of types where we have a logger inside them somewhere
class Logging a where
  -- | Returns a channel for logging and an Identifying string to use
  getLogger :: a -> (String, LogChannel)

instance Logging LogChannel where
  getLogger ch = ("Unknown", ch)


-- TODO: Consider generalizing this to any member of Show
data LogMsg = Mes LogPriority String String

instance Show LogMsg where
    show (Mes pri name str) = show name ++ "(" ++ show pri ++ "):\t" ++ str

type LogChannel = Channel LogMsg

-- | If a process has access to a logging channel, it is able to log messages to the world
log :: Logging a => LogPriority -> String -> Process a b ()
log prio _   | prio < logLevel = return ()
log prio msg | otherwise = do
		(name, logC) <- asks getLogger
		liftIO $ logMsg' logC name prio msg

logInfo, logDebug, logFatal, logWarn, logError :: Logging a => String -> Process a b ()
logInfo  = log Info
logDebug = log Debug
logFatal = log Fatal
logWarn  = log Warn
logError = log Error

-- | Log a message to a channel
logMsg :: LogChannel -> String -> IO ()
logMsg c m = logMsg' c "Unknown" Info m

-- | Log a message to a channel with a priority
logMsg' :: LogChannel -> String -> LogPriority -> String -> IO ()
logMsg' c name pri | pri < logLevel = const $ return ()
                   | otherwise      = sync . transmit c . Mes pri name
