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
		 | Error -- ^ Errors that are continuable
		 | Warn  -- ^ Potentially harmful situations
		 | Fatal -- ^ Severe errors. Will probably make the application abort
		 | Info  -- ^ Informational messages, progress reports
		    deriving Show

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
log prio msg = do
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
logMsg' c name pri = sync . transmit c . Mes pri name
