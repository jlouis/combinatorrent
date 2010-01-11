-- | Erlang style supervisors for Haskell.
--   Note that yet, these are not really good enough for using in other projects.
--   are currently subject to change until I figure out how a nice interface will
--   look like. At that moment they could be split off into their own package.
module Supervisor
  ( allForOne
  , oneForOne
  , Child(..)
  , Children
  , SupervisorMsg(..)
  , SupervisorChan
  )
where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.CML
import Control.Exception
import Control.Monad

import Data.List

import Prelude hiding (catch)

data Child = Supervisor (SupervisorChan -> IO ThreadId)
           | Worker     (SupervisorChan -> IO ThreadId)

data SupervisorMsg = IAmDying ThreadId
		   | PleaseDie ThreadId
		   | SpawnNew Child

type SupervisorChan = Channel SupervisorMsg
type Children = [Child]

data ChildInfo = HSupervisor ThreadId
	       | HWorker ThreadId

-- | Run a set of processes and do it once in the sense that if someone dies,
--   no restart is attempted. We will just kill off everybody without any kind
--   of prejudice.
allForOne :: Children -> SupervisorChan -> IO ThreadId
allForOne children parentC = do
    c <- channel
    childs <- mapM (spawnChild c) children
    spawn $ eventLoop childs c
  where eventLoop childs c = do mTid <- myThreadId
				sync $ choose [childEvent c childs,
					       parentEvent mTid c childs]
	childEvent c childs = wrap (receive c (const True))
	    (\msg -> case msg of
	       IAmDying tid -> do mapM_ (finChild c) childs
			          t <- myThreadId
				  sync $ transmit parentC (IAmDying t)
	       SpawnNew chld -> do n <- spawnChild c chld
				   eventLoop (n : childs) c)
	parentEvent mTid c childs = wrap (receive parentC (\(PleaseDie tid) -> tid == mTid))
	    (\msg -> case msg of
	       PleaseDie _ -> do mapM_ (finChild c) childs
			         return ())


-- | A One-for-one supervisor is called with @oneForOne children parentCh@. It will spawn and run
--   @children@ and be linked into the supervisor structure on @parentCh@. It returns the ThreadId
--   of the supervisor itself and the Channel of which it is the controller.
--
--   Should a process die, the one-for-one supervisor will do nothing about it. It will just record
--   the death and let the other processes keep running.
--
--   TODO: Restart policies.
oneForOne :: Children -> SupervisorChan -> IO (ThreadId, SupervisorChan)
oneForOne children parentC = do
    c <- channel
    childs <- mapM (spawnChild c) children
    tid <- spawn $ eventLoop childs c
    return (tid, c)
  where eventLoop childs c = do
	    mTid <- myThreadId
	    sync $ choose [childEvent c childs,
			   parentEvent mTid c childs]
	childEvent c childs = wrap (receive c (const True))
	    (\msg -> case msg of
		        IAmDying tid -> eventLoop (pruneChild tid childs) c
			SpawnNew chld -> do n <- spawnChild c chld
					    eventLoop (n : childs) c)
	parentEvent mTid c childs = wrap (receive parentC (\(PleaseDie tid) -> tid == mTid))
				    (\msg -> mapM_ (finChild c) childs)
	pruneChild tid childs = filter check childs
	  where check (HSupervisor t) = t == tid
	        check (HWorker t)     = t == tid

finChild :: SupervisorChan -> ChildInfo -> IO ()
finChild _ (HWorker tid) = killThread tid
finChild c (HSupervisor tid) = sync $ transmit c (PleaseDie tid)

spawnChild :: SupervisorChan -> Child -> IO ChildInfo
spawnChild c (Worker proc)     = HWorker <$> proc c
spawnChild c (Supervisor proc) = HSupervisor <$> proc c
