module ChokeMgrP (
    -- * Types, Channels
      ChokeMgrChannel
    -- * Interface
    , start
    -- * Helper functions
    , removePeer
    , addPeer
    )
where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Data.Traversable as T

import Control.Concurrent
import Control.Concurrent.CML
import Control.Exception

import Prelude hiding (catch)

import System.Random

import PeerP
import PeerTypes
import PieceMgrP hiding (start)
import ConsoleP hiding (start)
import FSP hiding (start, State)
import Supervisor
import Torrent hiding (infoHash)
import TimerP

-- DATA STRUCTURES
----------------------------------------------------------------------

data ChokeMgrMsg = Tick
		 | RemovePeer PeerPid
		 | AddPeer PeerPid PeerChannel Bool
type ChokeMgrChannel = Channel ChokeMgrMsg

data State = State
    { logCh :: LogChannel
    , mgrCh :: ChokeMgrChannel
    , peerDB :: PeerDB
    , uploadSlots :: Int
    }

-- INTERFACE
----------------------------------------------------------------------

start :: LogChannel -> ChokeMgrChannel -> Int -> SupervisorChan -> IO ThreadId
start logC ch ur supC = do
    TimerP.register 10 Tick ch
    spawn $ startup $ State logC ch initPdb (calcUploadSlots ur Nothing)
  where startup s = Supervisor.defaultStartup supC "ChokeMgr" (lp s)
        initPdb = PeerDB 2 M.empty []
        lp s = sync (mgrEvent s) >>= lp
        mgrEvent s = wrap (receive (mgrCh s) (const True))
                                   (\msg -> case msg of
				      Tick -> tick s
				      RemovePeer t -> removePeer t s
				      AddPeer t pCh weSeed -> addP t pCh weSeed s)
	tick s = do logMsg logC "Ticked"
		    TimerP.register 10 Tick (mgrCh s)
		    update s >>= rechoke
	withPeerDB f s = do db' <- f (peerDB s)
			    return s { peerDB = db' }
	removePeer tid = withPeerDB (\db -> return $ db { peerMap = M.delete tid (peerMap db) })
	addP tid pCh weSeed s = withPeerDB
		(\db -> getStdRandom (addPeer' db pCh weSeed tid)) s
	update = withPeerDB updateDB
	rechoke s = withPeerDB (flip runRechokeRound (uploadSlots s)) s

addPeer :: ChokeMgrChannel -> PeerPid -> PeerChannel -> Bool -> IO ()
addPeer ch pid pch = sync . transmit ch . (AddPeer pid pch)

removePeer :: ChokeMgrChannel -> PeerPid -> IO ()
removePeer ch pid = sync $ transmit ch $ RemovePeer pid

-- INTERNAL FUNCTIONS
----------------------------------------------------------------------

type PeerPid = ThreadId -- For now, should probably change


-- | The PeerDB is the database we keep over peers. It maps all the information necessary to determine
--   which peers are interesting to keep uploading to and which are slow. It also keeps track of how
--   far we are in the process of wandering the optimistic unchoke chain.
data PeerDB = PeerDB
    { chokeRound :: Int       -- ^ Counted down by one from 2. If 0 then we should advance the peer chain.
    , peerMap :: PeerMap      -- ^ Map of peers
    , peerChain ::  [PeerPid] -- ^ The order in which peers are optimistically unchoked
    }

-- | The PeerInfo structure maps, for each peer pid, its accompanying informative data for the PeerDB
data PeerInfo = PeerInfo
      { pChokingUs :: Bool
      , pDownRate :: Double -- ^ The rate of the peer in question, bytes downloaded in last window
      , pChannel :: PeerChannel -- ^ The channel on which to communicate with the peer
      , pInterestedInUs :: Bool -- ^ Reflection from Peer DB
      , pAreSeeding :: Bool -- ^ True if this peer is connected on a torrent we seed
      , pIsASeeder :: Bool -- ^ True if the peer is a seeder
      }

type PeerMap = M.Map PeerPid PeerInfo

-- | Auxilliary data structure. Used in the rechoking process.
type RechokeData = (PeerPid, PeerInfo)

-- | Peers are sorted by their current download rate. We want to keep fast peers around.
sortPeers :: [RechokeData] -> [RechokeData]
sortPeers = sortBy (comparing $ pDownRate . snd)

-- | Advance the peer chain to the next peer eligible for optimistic
--   unchoking. That is, skip peers which are not interested in our pieces
--   and peers which are not choking us. The former we can't send any data to,
--   so we can't get better speeds at them. The latter are already sending us data,
--   so we know how good they are as peers.
advancePeerChain :: [PeerPid] -> PeerMap -> [PeerPid]
advancePeerChain [] mp = []
advancePeerChain peers mp = back ++ front
  where (front, back) = break (\p -> isInterested p mp && isChokingUs p mp) peers

-- | Add a peer to the Peer Database
addPeer' :: PeerDB -> PeerChannel -> Bool -> PeerPid -> StdGen -> (PeerDB, StdGen)
addPeer' db pCh weSeeding tid gen =
    (db { peerMap = M.insert tid initialPeerInfo (peerMap db)
        , peerChain = nChain }, gen')
  where
    (nChain, gen')  = addPeerChain gen tid (peerChain db)
    initialPeerInfo = PeerInfo { pChokingUs = True
			       , pDownRate = 0.0
			       , pChannel = pCh
			       , pInterestedInUs = False
			       , pAreSeeding = weSeeding -- TODO: Update this on torrent completion
			       , pIsASeeder = False -- May get updated quickly
			       }

-- | Insert a Peer randomly into the Peer chain. Threads the random number generator
--   through.
addPeerChain :: StdGen -> PeerPid -> [PeerPid] -> ([PeerPid], StdGen)
addPeerChain gen pid ls = (front ++ pid : back, gen')
  where (front, back) = splitAt pt ls
        (pt, gen') = randomR (0, length ls - 1) gen

-- | Predicate. Is the peer interested in any of our pieces?
isInterested :: PeerPid -> PeerMap -> Bool
isInterested p = pInterestedInUs . fromJust . M.lookup p

-- | Predicate. Is the peer choking us?
isChokingUs :: PeerPid -> PeerMap -> Bool
isChokingUs p = pChokingUs . fromJust . M.lookup p

-- | Calculate the amount of upload slots we have available. If the
--   number of slots is explicitly given, use that. Otherwise we
--   choose the slots based the current upload rate set. The faster
--   the rate, the more slots we allow.
calcUploadSlots :: Int -> Maybe Int -> Int
calcUploadSlots _ (Just n) = n
calcUploadSlots rate Nothing | rate <= 0 = 7 -- This is just a guess
                             | rate <  9 = 2
                             | rate < 15 = 3
                             | rate < 42 = 4
                             | otherwise = round . sqrt $ fromIntegral rate * 0.6

-- | The call @assignUploadSlots c ds ss@ will assume that we have @c@
--   slots for uploading at our disposal. The list @ds@ will be peers
--   that we would like to upload to among the torrents we are
--   currently downloading. The list @ss@ is the same thing but for
--   torrents that we seed. The function returns a pair @(kd,ks)@
--   where @kd@ is the number of downloader slots and @ks@ is the
--   number of seeder slots.
--
--   The function will move surplus slots around so all of them gets used.
assignUploadSlots :: Int -> [RechokeData] -> [RechokeData] -> (Int, Int)
assignUploadSlots slots downloaderPeers seederPeers =
    -- Shuffle surplus slots around so all gets used
    shuffleSeeders downloaderPeers seederPeers $ shuffleDownloaders
                                                   downloaderPeers
                                                   (downloaderSlots, seederSlots)
  where
    -- Calculate the slots available for the downloaders and seeders
    downloaderSlots = max 1 $ round $ fromIntegral slots * 0.7
    seederSlots     = max 1 $ round $ fromIntegral slots * 0.3

    -- If there is a surplus of downloader slots, then assign them to
    --  the seeder slots
    shuffleDownloaders dPeers (dSlots, sSlots) =
        case max 0 (dSlots - length dPeers) of
          0 -> (dSlots, sSlots)
          k -> (dSlots - k, sSlots + k)

    -- If there is a surplus of seeder slots, then assign these to
    --   the downloader slots. Limit the downloader slots to the number
    --   of downloaders, however
    shuffleSeeders dPeers sPeers (dSlots, sSlots) =
        case max 0 (sSlots - length sPeers) of
          0 -> (dSlots, sSlots)
          k -> (min (dSlots + k) (length dPeers), sSlots - k)

-- | @selectPeers upRate d s@ selects peers from a list of downloader peers @d@ and a list of seeder
--   peers @s@. The value of @upRate@ defines the upload rate for the client and is used in determining
--   the rate of the slots.
selectPeers :: Int -> [RechokeData] -> [RechokeData] -> S.Set PeerPid
selectPeers uploadSlots downPeers seedPeers = S.union downPids seedPids
    where
      (nDownSlots, nSeedSlots) = assignUploadSlots
                                 uploadSlots
                                 downPeers
                                 seedPeers
      downPids = S.fromList $ map fst $ take nDownSlots downPeers
      seedPids = S.fromList $ map fst $ take nSeedSlots seedPeers

-- | This function carries out the choking and unchoking of peers in a round.
performChokingUnchoking :: S.Set PeerPid -> [RechokeData] -> IO ()
performChokingUnchoking elected peers =
    do mapM_ (unchoke . snd) unchokers
       optChoke defaultOptimisticSlots chokers
  where
    -- Partition the peers based on they were selected or not
    (unchokers, chokers) = partition (\rd -> S.member (fst rd) elected) peers
    -- Choke and unchoke helpers.
    --   If we block on the sync, it means that the process in the other end must
    --   be dead. Thus we can just skip it. We will eventually receive this knowledge
    --   through another channel.
    unchoke pi = unchokePeer (pChannel pi)
		    `catch` (\BlockedOnDeadMVar -> return ())
    choke   pi = chokePeer (pChannel pi)
		    `catch` (\BlockedOnDeadMVar -> return ())
    -- If we have k optimistic slots, @optChoke k peers@ will unchoke the first @k@ interested
    --  in us. The rest will either be unchoked if they are not interested (ensuring fast start
    --    should they become interested); or they will be choked to avoid TCP/IP congestion.
    optChoke _ [] = return ()
    optChoke 0 ((_, pi) : ps) = do if pInterestedInUs pi
                                     then chokePeer (pChannel pi)
                                     else unchokePeer (pChannel pi)
                                   optChoke 0 ps
    optChoke k ((_, pi) : ps) = if pInterestedInUs pi
                                then unchokePeer (pChannel pi) >> optChoke (k-1) ps
                                else unchokePeer (pChannel pi) >> optChoke k ps

-- | Function to split peers into those where we are seeding and those were we are leeching.
--   also prunes the list for peers which are not interesting.
--   TODO: Snubbed peers
splitSeedLeech :: [RechokeData] -> ([RechokeData], [RechokeData])
splitSeedLeech ps = partition (pAreSeeding . snd) $ filter picker ps
  where
    picker (_, pi) = not (pIsASeeder pi) || pInterestedInUs pi


buildRechokeData :: PeerDB -> [RechokeData]
buildRechokeData db = map cPeer (peerChain db)
  where cPeer pid = (pid, fromJust $ M.lookup pid (peerMap db))

rechoke :: PeerDB -> Int -> IO ()
rechoke db uploadSlots = performChokingUnchoking electedPeers peers
  where
    peers = buildRechokeData db
    (down, seed) = splitSeedLeech peers
    electedPeers = selectPeers uploadSlots down seed

updateDB :: PeerDB -> IO PeerDB
updateDB db = do nmp <- T.mapM gatherRate $ peerMap db
                 return db { peerMap = nmp }
    where
      gatherRate pi = do ch <- channel
			 (do sync $ transmit (pChannel pi) $ PeerStats ch
			     (rt, interested) <- sync $ receive ch (const True)
                             return pi { pDownRate = rt,
				         pInterestedInUs = interested })
			    `catch` (\BlockedOnDeadMVar -> return pi) -- Peer dead, ignore it.

runRechokeRound :: PeerDB -> Int -> IO PeerDB
runRechokeRound db uploadSlots = do
    let db' = if chokeRound db == 0
                    then db { peerChain = advancePeerChain (peerChain db) (peerMap db),
                              chokeRound = 2 }
                    else db { chokeRound = chokeRound db - 1 }
    rechoke db' uploadSlots
    return db'
