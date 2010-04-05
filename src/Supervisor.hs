-- | Erlang style supervisors for Haskell.
--   Note that yet, these are not really good enough for using in other projects.
--   are currently subject to change until I figure out how a nice interface will
--   look like. At that moment they could be split off into their own package.
{-# LANGUAGE ScopedTypeVariables #-}
module Supervisor (
    -- * Types
    Child(..)
  , Children
  , SupervisorMsg(..)
  , SupervisorChan
    -- * Supervisor Initialization
  , allForOne
  , oneForOne
    -- * helper calls
  , pDie
  , defaultStopHandler
  )
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Reader

import Prelude hiding (catch)

import Process

data Child = Supervisor (SupervisorChan -> IO ThreadId)
           | Worker     (SupervisorChan -> IO ThreadId)

data SupervisorMsg = IAmDying ThreadId
                   | PleaseDie ThreadId
                   | SpawnNew Child

type SupervisorChan = TChan SupervisorMsg
type Children = [Child]

data ChildInfo = HSupervisor ThreadId
               | HWorker ThreadId


pDie :: SupervisorChan -> IO ()
pDie supC = do
    tid <- myThreadId
    atomically $ writeTChan supC (IAmDying tid)

class SupervisorConf a where
    getParent :: a -> SupervisorChan
    getChan   :: a -> SupervisorChan

data CFOFA = CFOFA { name :: String
                   , chan :: SupervisorChan
                   , parent :: SupervisorChan
                   }

instance SupervisorConf CFOFA where
    getParent = parent
    getChan   = chan

instance Logging CFOFA where
    logName = name

data STOFA = STOFA { childInfo :: [ChildInfo] }

-- | Run a set of processes and do it once in the sense that if someone dies,
--   no restart is attempted. We will just kill off everybody without any kind
--   of prejudice.
allForOne :: String -> Children -> SupervisorChan -> IO ThreadId
allForOne n children parentC = do
    c <- newTChanIO
    spawnP (CFOFA n c parentC) (STOFA []) (catchP startup
                                             (defaultStopHandler parentC))
  where
    startup = do
        childs <- mapM spawnChild children
        modify (\_ -> STOFA (reverse childs))
        forever eventLoop
    eventLoop = do
        mTid <- liftIO myThreadId
        pc <- asks parent
        ch <- asks chan
        m <- liftIO . atomically $
            (readTChan ch >>= return . Left) `orElse`
            (readTChan pc >>= return . Right)
        case m of
            Left ev -> case ev of
                        IAmDying _tid -> do
                            gets childInfo >>= mapM_ finChild
                            t <- liftIO myThreadId
                            asks parent >>= \c -> liftIO . atomically $ writeTChan c (IAmDying t)
                        SpawnNew chld -> do
                            nc <- spawnChild chld
                            modify (\(STOFA cs) -> STOFA (nc : cs))
                        _ -> fail "Impossible"
            Right ev -> case ev of
                PleaseDie tid | tid == mTid -> do
                    gets childInfo >>= mapM_ finChild
                    stopP
                _                           -> return ()

data CFOFO = CFOFO { oName :: String
                   , oChan :: SupervisorChan
                   , oparent :: SupervisorChan
                   }

instance SupervisorConf CFOFO where
    getParent = oparent
    getChan   = oChan

instance Logging CFOFO where
    logName = oName

data STOFO = STOFO { oChildInfo :: [ChildInfo] }

-- | A One-for-one supervisor is called with @oneForOne children parentCh@. It will spawn and run
--   @children@ and be linked into the supervisor structure on @parentCh@. It returns the ThreadId
--   of the supervisor itself and the Channel of which it is the controller.
--
--   Should a process die, the one-for-one supervisor will do nothing about it. It will just record
--   the death and let the other processes keep running.
--
--   TODO: Restart policies.
oneForOne :: String -> Children -> SupervisorChan -> IO (ThreadId, SupervisorChan)
oneForOne n children parentC = do
    c <- newTChanIO
    t <- spawnP (CFOFO n c parentC) (STOFO []) (catchP startup
                                                (defaultStopHandler parentC))
    return (t, c)
  where
    startup :: Process CFOFO STOFO ()
    startup = do
        childs <- mapM spawnChild children
        modify (\_ -> STOFO (reverse childs))
        forever eventLoop
    eventLoop :: Process CFOFO STOFO ()
    eventLoop = do
        mTid <- liftIO myThreadId
        pc <- asks oparent
        ch <- asks oChan
        m <- liftIO . atomically $
            (readTChan ch >>= return . Left) `orElse`
            (readTChan pc >>= return . Right)
        case m of
            Left ev -> case ev of
                    IAmDying tid -> pruneChild tid
                    SpawnNew chld -> do nc <- spawnChild chld
                                        modify (\(STOFO cs) -> STOFO (nc : cs))
                    _ -> fail "Impossible (2)"
            Right ev -> case ev of
                PleaseDie tid | tid == mTid -> do
                    gets oChildInfo >>= mapM_ finChild
                    stopP
                _                           -> return ()
    pruneChild tid = modify (\(STOFO cs) -> STOFO (filter chk cs))
          where chk (HSupervisor t) = t == tid
                chk (HWorker t)     = t == tid


finChild :: SupervisorConf a => ChildInfo -> Process a b ()
finChild (HWorker tid) = liftIO $ killThread tid -- Make this call killP in Process?
finChild (HSupervisor tid) = do
    c <- getChan <$> ask
    liftIO . atomically $ writeTChan c (PleaseDie tid)

spawnChild :: SupervisorConf a => Child -> Process a b ChildInfo
spawnChild (Worker proc)     = do
    c <- getChan <$> ask
    nc <- liftIO . atomically $ dupTChan c
    tid <- liftIO $ proc nc
    return $ HWorker tid
spawnChild (Supervisor proc) = do
    c <- getChan <$> ask
    nc <- liftIO . atomically $ dupTChan c
    tid <- liftIO $ proc nc
    return $ HSupervisor tid

defaultStopHandler :: SupervisorChan -> Process a b ()
defaultStopHandler supC = do
    t <- liftIO $ myThreadId
    liftIO . atomically $ writeTChan supC $ IAmDying t

