-- | Logging primitives
{-# LANGUAGE TypeSynonymInstances #-}
module Logging (
  -- * Classes
    Logging(..)
  -- * Types
  , LogChannel
  , LogPriority(..)
  -- * Spawning
  , startLogger
  -- * Interface (deprecated)
  , logMsg
  , logMsg'
  )
where

import Control.Concurrent
import Control.Concurrent.CML
import Control.Monad.Reader

import Data.Monoid

import LoggingTypes
import Prelude hiding (log)

import Process

startLogger :: LogChannel -> IO ThreadId
startLogger logC =
    spawnP logC () (forever lp)
  where
    lp = do m <- syncP =<< logEv
            liftIO $ print m
    logEv = recvP logC (const True)

-- | Log a message to a channel
logMsg :: LogChannel -> String -> IO ()
logMsg c m = logMsg' c "Unknown" Info m

-- | Log a message to a channel with a priority
logMsg' :: LogChannel -> String -> LogPriority -> String -> IO ()
logMsg' c name pri = sync . transmit c . Mes pri name
