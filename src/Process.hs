-- | Core Process code
{-# LANGUAGE ExistentialQuantification, FlexibleInstances,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, CPP #-}
-- required for deriving Typeable
{-# OPTIONS_GHC -fglasgow-exts #-}

module Process (
    -- * Types
      Process
    -- * Interface
    , runP
    , spawnP
    , catchP
    , cleanupP
    , stopP
    -- * Log Interface
    , Logging(..)
    , logP
    , infoP
    , debugP
    , warningP
    , criticalP
    , errorP
    )
where

import Control.Concurrent
import Control.Exception

import Control.Monad.Reader
import Control.Monad.State

import Data.Typeable

import Prelude hiding (catch, log)

import System.Log.Logger

-- | A @Process a b c@ is the type of processes with access to configuration data @a@, state @b@
--   returning values of type @c@. Usually, the read-only data are configuration parameters and
--   channels, and the state the internal process state. It is implemented by means of a transformer
--   stack on top of IO.
newtype Process a b c = Process (ReaderT a (StateT b IO) c)
#ifndef __HADDOCK__
  deriving (Functor, Monad, MonadIO, MonadState b, MonadReader a, Typeable)
#endif

data StopException = StopException
  deriving (Show, Typeable)

instance Exception StopException

stopP :: Process a b c
stopP = throw StopException

-- | Run the process monad given a configuation of type @a@ and a initial state of type @b@
runP :: a -> b -> Process a b c -> IO (c, b)
runP c st (Process p) = runStateT (runReaderT p c) st

-- | Spawn and run a process monad
spawnP :: a -> b -> Process a b () -> IO ThreadId
spawnP c st p = forkIO proc
  where proc = do _ <- runP c st p
                  return ()

-- | Run the process monad for its side effect, with a stopHandler if exceptions
--   are raised in the process
catchP :: Logging a => Process a b () -> Process a b () -> Process a b ()
catchP proc stopH = cleanupP proc stopH (return ())

-- | Run the process monad for its side effect. @cleanupP p sh ch@ describes to
--   run @p@. If @p@ dies by a kill from a supervisor, run @ch@. Otherwise it runs
--   @ch >> sh@ on death.
cleanupP :: Logging a => Process a b () -> Process a b () -> Process a b () -> Process a b ()
cleanupP proc stopH cleanupH = do
  st <- get
  c  <- ask
  (a, s') <- liftIO $ runP c st proc `catches`
                [ Handler (\ThreadKilled ->
                    runP c st ( do infoP $ "Process Terminated by Supervisor"
                                   cleanupH ))
                , Handler (\StopException ->
                     runP c st (do infoP $ "Process Terminating gracefully"
                                   cleanupH >> stopH)) -- This one is ok
                , Handler (\(ex :: SomeException) ->
                    runP c st (do criticalP $ "Process exiting due to ex: " ++ show ex
                                  cleanupH >> stopH))
                ]
  put s'
  return a

------ LOGGING

--
-- | The class of types where we have a logger inside them somewhere
class Logging a where
  -- | Returns a channel for logging and an Identifying string to use
  logName :: a -> String

logP :: Logging a => Priority -> String -> Process a b ()
logP prio msg = do
    n <- asks logName
    liftIO $ logM n prio (n ++ ":\t" ++ msg)

infoP, debugP, criticalP, warningP, errorP :: Logging a => String -> Process a b ()
infoP  = logP INFO
debugP = logP DEBUG
criticalP = logP CRITICAL
warningP  = logP WARNING
errorP    = logP ERROR

