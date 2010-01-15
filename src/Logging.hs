-- | Logging primitives
module Logging
  -- * Types
  ( LogChannel
  , LogPriority(..)
  -- * Interface
  , logMsg
  , logMsg'
  , logFatal
  )
where

import Control.Concurrent.CML

data LogPriority = Low
                 | Default
                 | System
                 | High
                 | Fatal
                 deriving Show

-- TODO: Consider generalizing this to any member of Show
data LogMsg = Mes LogPriority String

instance Show LogMsg where
    show (Mes pri str) = show pri ++ ":\t" ++ str

type LogChannel = Channel LogMsg

-- | Log a message to a channel
logMsg :: LogChannel -> String -> IO ()
logMsg c = sync . transmit c . Mes Default

-- | Log a fatal message on a channel, TODO: use logMsg' for this
logFatal :: LogChannel -> String -> IO ()
logFatal c = sync . transmit c . Mes Fatal

-- | Log a message to a channel with a priority
logMsg' :: LogChannel -> LogPriority -> String -> IO ()
logMsg' c pri = sync . transmit c . Mes pri
