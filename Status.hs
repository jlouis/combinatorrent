module Status (TorrentState(..),
               State(uploaded, downloaded, state),
               status)
where

import Control.Concurrent.CHP
import Control.Monad()
import Control.Monad.Trans()

data TorrentState = Seeding | Leeching

data State = MkState { uploaded :: Integer,
                       downloaded :: Integer,
                       state :: TorrentState }

status :: TorrentState -> Chanout State -> CHP ()
status tstate trackerChan = lp $ MkState 0 0 tstate
  where lp s =
            do waitFor (10*1000000) >> (writeChannel trackerChan s)
               lp s
