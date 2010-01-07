-- | Erlang style supervisors for Haskell.
--   Note that yet, these are not really good enough for using in other projects.
--   are currently subject to change until I figure out how a nice interface will
--   look like. At that moment they could be split off into their own package.
module Supervisor
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

	finChild :: SupervisorChan -> ChildInfo -> IO ()
	finChild _ (HWorker tid) = killThread tid
	finChild c (HSupervisor tid) = sync $ transmit c (PleaseDie tid)

	spawnChild :: SupervisorChan -> Child -> IO ChildInfo
	spawnChild c (Worker proc)     = HWorker <$> proc c
	spawnChild c (Supervisor proc) = HSupervisor <$> proc c

-- | Run a set of processes. If one dies, then let the others run on as if
--   Nothing happened.
{-
oneForOne :: Children -> SupervisorChan -> IO ThreadId
oneForOne children parentC = do
    c <- channel
    childs <- mapM (spawnChild c) children
    spawn $ eventLoop childs c
  where eventLoop childs c = do
	    msg <- sync $ receive c (const True)
	    case msg of
		IAmDying tid -> eventLoop (childs \\ [tid]) c
		SpawnNew proc -> do nTid <- proc c
				    eventLoop (nTid : childs) c
		PleaseDie -> do finalize childs
				return ()
	finalize childs = mapM_ killThread childs
	spawnChild c (Worker proc) = proc c
-}

-- TODO: Thinking hat on, we need to understand these details some more.
--
