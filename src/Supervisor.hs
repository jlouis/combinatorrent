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
  , SupervisorChannel
    -- * Supervisor Initialization
  , allForOne
  , oneForOne
    -- * helper calls
  , pDie
  , defaultStopHandler
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Reader

import Prelude

import Process

data Child = Supervisor (SupervisorChannel -> IO (ThreadId, SupervisorChannel))
           | Worker     (SupervisorChannel -> IO ThreadId)

instance Show Child where
    show (Supervisor _) = "Supervisor"
    show (Worker _)      = "Worker"

data SupervisorMsg = IAmDying ThreadId
                   | PleaseDie ThreadId
                   | SpawnNew Child
  deriving Show

type SupervisorChannel = TChan SupervisorMsg
type Children = [Child]

data ChildInfo = HSupervisor ThreadId
               | HWorker ThreadId

data RestartPolicy = AllForOne | OneForOne

pDie :: SupervisorChannel -> IO ()
pDie supC = do
    tid <- myThreadId
    atomically $ writeTChan supC (IAmDying tid)

data CF = CF { name :: String              -- ^ Name of the supervisor
             , chan :: SupervisorChannel   -- ^ Channel of the supervisor
             , parent :: SupervisorChannel -- ^ Channel of the parent supervisor
             , restartPolicy :: RestartPolicy }

instance Logging CF where
    logName = name

data ST = ST { childInfo :: [ChildInfo] }

start :: RestartPolicy -> String -> Children -> SupervisorChannel -> IO (ThreadId,
                                                                         SupervisorChannel)
start policy n children parentC = do
    c <- newTChanIO
    t <- spawnP (CF n c parentC policy) (ST []) (catchP (startup children)
                                              (defaultStopHandler parentC))
    return (t, c)

startup :: [Child] -> Process CF ST ()
startup children = do
    spawnedChildren <- mapM spawnChild children
    put $ ST (reverse spawnedChildren)
    forever eventLoop

eventLoop :: Process CF ST ()
eventLoop = do
    mTid <- liftIO myThreadId
    pc   <- asks parent
    ch   <- asks chan
    m <- liftIO . atomically $
        (readTChan ch >>= return . Left) `orElse`
        (readTChan pc >>= return . Right)
    case m of
        Left (IAmDying tid) -> handleIAmDying tid
        Left (SpawnNew chld) -> handleSpawnNew chld
        Right (PleaseDie tid) | tid == mTid -> handlePleaseDie
        _ -> return () -- Ignore these. Since the chan is duped, we get stray messages from above

handleIAmDying :: ThreadId -> Process CF ST ()
handleIAmDying tid = do
    p <- asks restartPolicy
    case p of
        AllForOne -> do
            gets childInfo >>= mapM_ finChild
            stopP
        OneForOne ->
            pruneChild tid

handleSpawnNew :: Child -> Process CF ST ()
handleSpawnNew chld = do
   nc <- spawnChild chld
   modify (\(ST cs) -> ST (nc : cs))

handlePleaseDie :: Process CF ST ()
handlePleaseDie = do
    gets childInfo >>= mapM_ finChild
    stopP


pruneChild :: ThreadId -> Process CF ST ()
pruneChild tid = modify (\(ST cs) -> ST (filter chk cs))
    where chk (HSupervisor t) = t == tid
          chk (HWorker t)     = t == tid

-- | A One-for-one supervisor is called with @oneForOne children parentCh@. It will spawn and run
--   @children@ and be linked into the supervisor structure on @parentCh@. It returns the ThreadId
--   of the supervisor itself and the Channel of which it is the controller.
--
--   Should a process die, the one-for-one supervisor will do nothing about it. It will just record
--   the death and let the other processes keep running.
--
--   TODO: Restart policies.
oneForOne :: String -> Children -> SupervisorChannel -> IO (ThreadId, SupervisorChannel)
oneForOne = start OneForOne


-- | Run a set of processes and do it once in the sense that if someone dies,
--   no restart is attempted. We will just kill off everybody without any kind
--   of prejudice.
allForOne :: String -> Children -> SupervisorChannel -> IO (ThreadId, SupervisorChannel)
allForOne = start AllForOne


finChild :: ChildInfo -> Process CF ST ()
finChild (HWorker tid) = liftIO $ killThread tid
finChild (HSupervisor tid) = do
    c <- asks chan
    liftIO . atomically $ writeTChan c (PleaseDie tid)

spawnChild :: Child -> Process CF ST ChildInfo
spawnChild (Worker proc)     = do
    c <- asks chan
    nc <- liftIO . atomically $ dupTChan c
    tid <- liftIO $ proc nc
    return $ HWorker tid
spawnChild (Supervisor proc) = do
    c <- asks chan
    nc <- liftIO . atomically $ dupTChan c
    (tid, _) <- liftIO $ proc nc
    return $ HSupervisor tid

defaultStopHandler :: SupervisorChannel -> Process a b ()
defaultStopHandler supC = do
    t <- liftIO $ myThreadId
    liftIO . atomically $ writeTChan supC $ IAmDying t

