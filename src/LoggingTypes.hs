{-# LANGUAGE TypeSynonymInstances #-}
module LoggingTypes
   ( Logging(..)
   , LogPriority(..)
   , LogMsg(..)
   , LogChannel
   )
where

import Control.Concurrent.CML


data LogPriority = Debug -- ^ Fine grained debug info
                 | Warn  -- ^ Potentially harmful situations
                 | Info  -- ^ Informational messages, progress reports
                 | Error -- ^ Errors that are continuable
                 | Fatal -- ^ Severe errors. Will probably make the application abort
                 | None  -- ^ No logging at all
                    deriving (Show, Eq, Ord)
--
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
