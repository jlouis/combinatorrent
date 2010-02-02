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
    -- * Interface
    , logInfo
    , logDebug
    , logWarn
    , logFatal
    , logError
    )
where

import Data.Monoid
import Control.Applicative

import Control.Concurrent
import Control.Concurrent.CML
import Control.Exception

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Data.Typeable

import LoggingTypes
import Prelude hiding (catch, log)

import System.IO

-- import Supervisor


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
		    runP c st ( do logDebug $ "Process Terminating by Supervisor"
				   cleanupH ))
		, Handler (\StopException -> 
		     runP c st (do logDebug $ "Process Terminating gracefully"
				   cleanupH >> stopH)) -- This one is ok
		, Handler (\(ex :: SomeException) ->
		    runP c st (do logFatal $ "Process exiting due to ex: " ++ show ex
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
    return $ (wrap (transmit ch v)
		(\() -> return ((), s)))

sendPC :: (a -> Channel c) -> c -> Process a b (Event ((), b))
sendPC sel v = asks sel >>= flip sendP v

recvP :: Channel c -> (c -> Bool) -> Process a b (Event (c, b))
recvP ch pred = do
  s <- get
  return (wrap (receive ch pred)
	    (\v -> return (v, s)))

recvPC :: (a -> Channel c) -> Process a b (Event (c, b))
recvPC sel = asks sel >>= flip recvP (const True)

wrapP :: Event (c, b) -> (c -> Process a b y) -> Process a b (Event (y, b))
wrapP ev p = do
    c <- ask
    return $ wrap ev (\(v, s) -> runP c s (p v))

-- Convenience function
recvWrapPC :: (a -> Channel c) -> (c -> Process a b y) -> Process a b (Event (y, b))
recvWrapPC sel p = do
    ev <- recvPC sel
    wrapP ev p

chooseP :: [Process a b (Event (c, b))] -> Process a b (Event (c, b))
chooseP events = (sequence events) >>= (return . choose)

------ LOGGING




-- | If a process has access to a logging channel, it is able to log messages to the world
log :: Logging a => LogPriority -> String -> Process a b ()
log prio msg = do
	(name, logC) <- asks getLogger
	when (prio >= logLevel name)
		(liftIO $ logMsg' logC name prio msg)
  where logMsg' c name pri = sync . transmit c . Mes pri name

logInfo, logDebug, logFatal, logWarn, logError :: Logging a => String -> Process a b ()
logInfo  = log Info

-- Logging filters
type LogFilter = String -> LogPriority

matchP :: String -> LogPriority -> LogFilter
matchP process prio = \s -> if s == process then prio else None

matchAny :: LogPriority -> LogFilter
matchAny prio = const prio

matchNone :: LogFilter
matchNone = const None

instance Monoid LogFilter where
    mempty = const None
    mappend f g = \x ->
		let fx = f x
		in if fx /= None then fx else g x

-- | The level by which we log
logLevel :: LogFilter
#ifdef DEBUG
logLevel = matchAny Debug
#else
logLevel = matchAny Info
#endif

logDebug = log Debug
logFatal = log Fatal
logWarn  = log Warn
logError = log Error
