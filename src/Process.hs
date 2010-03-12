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
    , foreverP
    , syncP
    , chooseP
    , sendP
    , sendPC
    , recvP
    , recvPC
    , recvWrapPC
    , wrapP
    , stopP
    , ignoreProcessBlock -- This ought to be renamed
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
import Control.Concurrent.CML
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
spawnP c st p = spawn proc
  where proc = do runP c st p
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
                [ Handler (\ThreadKilled -> do
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

-- | Run a process forever in a loop
foreverP :: Process a b c -> Process a b c
foreverP p = p >> foreverP p

syncP :: Event (c, b) -> Process a b c
syncP ev = do (a, s) <- liftIO $ sync ev
              put s
              return a

sendP :: Channel c -> c -> Process a b (Event ((), b))
sendP ch v = do
    s <- get
    return $ (wrap (send ch v)
                (\() -> return ((), s)))
  where send = {-# SCC "transmit" #-} transmit

sendPC :: (a -> Channel c) -> c -> Process a b (Event ((), b))
sendPC sel v = asks sel >>= flip sendP v

recvP :: Channel c -> (c -> Bool) -> Process a b (Event (c, b))
recvP ch pred = do
    s <- get
    return (wrap (recv ch pred)
              (\v -> return (v, s)))
  where recv = {-# SCC "receive" #-} receive

recvPC :: (a -> Channel c) -> Process a b (Event (c, b))
recvPC sel = asks sel >>= flip recvP (const True)

wrapP :: Event (c, b) -> (c -> Process a b y) -> Process a b (Event (y, b))
wrapP ev p = do
    c <- ask
    return $ wp ev (\(v, s) -> runP c s (p v))
  where wp = {-# SCC "wrap" #-} wrap

-- Convenience function
recvWrapPC :: (a -> Channel c) -> (c -> Process a b y) -> Process a b (Event (y, b))
recvWrapPC sel p = do
    ev <- recvPC sel
    wrapP ev p

chooseP :: [Process a b (Event (c, b))] -> Process a b (Event (c, b))
chooseP events = (sequence events) >>= (return . chs)
  where chs = {-# SCC "choose" #-} choose

-- VERSION SPECIFIC PROCESS ORIENTED FUNCTIONS

-- | @ignoreProcessBlock err thnk@ runs a process action, ignoring blocks on dead
--   MVars. If the MVar is blocked, return the default value @err@.
ignoreProcessBlock :: c -> Process a b c -> Process a b c
ignoreProcessBlock err thnk = do
    st <- get
    c  <- ask
    (a, s') <-  liftIO $ runP c st thnk `catches`
    -- Peer dead, ignore
#if (__GLASGOW_HASKELL__ == 610)
        [ Handler (\BlockedOnDeadMVar -> return (err, st)) ]
#elif (__GLASGOW_HASKELL__ == 612)
        [ Handler (\BlockedIndefinitelyOnMVar -> return (err, st)) ]
#else
#error Unknown GHC version
#endif
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
    liftIO $ logM n prio msg

infoP, debugP, criticalP, warningP, errorP :: Logging a => String -> Process a b ()
infoP  = logP INFO
debugP = logP DEBUG
criticalP = logP CRITICAL
warningP  = logP WARNING
errorP    = logP ERROR

