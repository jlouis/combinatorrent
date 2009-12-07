module Status ()
where

data TorrentState = Seeding | Leeching

data State = MkState { uploaded :: Integer,
                       downloaded :: Integer,
                       state :: TorrentState }