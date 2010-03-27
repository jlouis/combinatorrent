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
import Control.Concurrent.CML.Strict
import Control.DeepSeq
import Control.Monad.State
import Control.Monad.Reader

import Prelude hiding (catch)

import Process

data Child = Supervisor (SupervisorChan -> IO ThreadId)
           | Worker     (SupervisorChan -> IO ThreadId)

data SupervisorMsg = IAmDying ThreadId
                   | PleaseDie ThreadId
                   | SpawnNew Child

instance NFData SupervisorMsg where
    rnf a = a `seq` ()

type SupervisorChan = Channel SupervisorMsg
type Children = [Child]

data ChildInfo = HSupervisor ThreadId
               | HWorker ThreadId


pDie :: SupervisorChan -> IO ()
pDie supC = do
    tid <- myThreadId
    sync $ transmit supC $ IAmDying tid

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
allForOne name children parentC = do
    c <- channel
    spawnP (CFOFA name c parentC) (STOFA []) (catchP startup
                                                        (defaultStopHandler parentC))
  where
    startup = do
        childs <- mapM spawnChild children
        modify (\_ -> STOFA (reverse childs))
        forever eventLoop
    eventLoop = do
        mTid <- liftIO myThreadId
        syncP =<< chooseP [childEvent, parentEvent mTid]
    childEvent = do
        ev <- recvPC chan
        wrapP ev (\msg -> case msg of
            IAmDying _tid -> do gets childInfo >>= mapM_ finChild
                                t <- liftIO myThreadId
                                sendPC parent (IAmDying t) >>= syncP
            SpawnNew chld -> do n <- spawnChild chld
                                modify (\(STOFA cs) -> STOFA (n : cs)))
    parentEvent mTid = do
        ev <- recvP parentC (\m -> case m of
                                    PleaseDie tid | tid == mTid -> True
                                    _                           -> False)
        wrapP ev (\msg -> case msg of
            PleaseDie _ -> gets childInfo >>= mapM_ finChild
            _           -> return ())

data CFOFO = CFOFO { oName :: String
                   , oChan :: SupervisorChan
                   , oparent :: SupervisorChan
                   }

instance SupervisorConf CFOFO where
    getParent = oparent
    getChan   = oChan

instance Logging CFOFO where
    logName = oName

data STOFO = STOFO [ChildInfo]

-- | A One-for-one supervisor is called with @oneForOne children parentCh@. It will spawn and run
--   @children@ and be linked into the supervisor structure on @parentCh@. It returns the ThreadId
--   of the supervisor itself and the Channel of which it is the controller.
--
--   Should a process die, the one-for-one supervisor will do nothing about it. It will just record
--   the death and let the other processes keep running.
--
--   TODO: Restart policies.
oneForOne :: String -> Children -> SupervisorChan -> IO (ThreadId, SupervisorChan)
oneForOne name children parentC = do
    c <- channel
    t <- spawnP (CFOFO name c parentC) (STOFO []) (catchP startup
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
        syncP =<< chooseP [childEvent, parentEvent mTid]
    childEvent = do
        ev <- recvPC oChan
        wrapP ev (\msg -> case msg of
                    IAmDying tid -> pruneChild tid
                    SpawnNew chld -> do n <- spawnChild chld
                                        modify (\(STOFO cs) -> STOFO (n : cs)))
    parentEvent mTid = do
        ev <- recvP parentC (\m -> case m of
                                     PleaseDie tid | tid == mTid -> True
                                     _                           -> False)
        wrapP ev (\_ -> do (STOFO cs) <- get
                           mapM_ finChild cs
                           stopP)
    pruneChild tid = modify (\(STOFO cs) -> STOFO (filter check cs))
          where check (HSupervisor t) = t == tid
                check (HWorker t)     = t == tid


finChild :: SupervisorConf a => ChildInfo -> Process a b ()
finChild (HWorker tid) = liftIO $ killThread tid -- Make this call killP in Process?
finChild (HSupervisor tid) = do
    c <- getChan <$> ask
    syncP =<< sendP c (PleaseDie tid)

spawnChild :: SupervisorConf a => Child -> Process a b ChildInfo
spawnChild (Worker proc)     = do
    c <- getChan <$> ask
    tid <- liftIO $ proc c
    return $ HWorker tid
spawnChild (Supervisor proc) = do
    c <- getChan <$> ask
    tid <- liftIO $ proc c
    return $ HSupervisor tid

defaultStopHandler :: SupervisorChan -> Process a b ()
defaultStopHandler supC = do
    t <- liftIO $ myThreadId
    syncP =<< (sendP supC $ IAmDying t)

