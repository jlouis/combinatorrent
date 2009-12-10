module Status (TorrentState(..),
               State(uploaded, downloaded, state),
               start)
where

import Control.Concurrent
import Control.Concurrent.CML

import Control.Monad()
import Control.Monad.Trans()

data TorrentState = Seeding | Leeching

data State = MkState { uploaded :: Integer,
                       downloaded :: Integer,
                       state :: TorrentState }

start :: TorrentState -> Channel State -> IO ()
start tstate trackerChan = lp $ MkState 0 0 tstate
  where lp s = do threadDelay (10 * 1000000)
                  sync $ transmit trackerChan s
                  lp s
