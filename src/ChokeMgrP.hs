module ChokeMgrP (
    -- * Types, Channels
      ChokeMgrChannel
    , ChokeMgrMsg(..)
    -- * Interface
    , start
    )
where

import Data.Time.Clock
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Data.Traversable as T

import Control.Concurrent
import Control.Concurrent.CML
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State


import Prelude hiding (catch, log)

import System.Random

import PeerP
import PeerTypes
import PieceMgrP hiding (start)
import Process
import Logging
import FSP hiding (start)
import Supervisor
import Torrent hiding (infoHash)
import TimerP

-- DATA STRUCTURES
----------------------------------------------------------------------

data ChokeMgrMsg = Tick
		 | RemovePeer PeerPid
		 | AddPeer PeerPid PeerChannel
type ChokeMgrChannel = Channel ChokeMgrMsg

data CF = CF { logCh :: LogChannel
	     , mgrCh :: ChokeMgrChannel
	     , infoCh :: ChokeInfoChannel
	     }

instance Logging CF where
  getLogger cf = ("ChokeMgrP", logCh cf)

type ChokeMgrProcess a = Process CF PeerDB a

-- INTERFACE
----------------------------------------------------------------------

start :: LogChannel -> ChokeMgrChannel -> ChokeInfoChannel -> Int -> Bool -> SupervisorChan -> IO ThreadId
start logC ch infoC ur weSeed supC = do
    TimerP.register 10 Tick ch
    spawnP (CF logC ch infoC) (initPeerDB $ calcUploadSlots ur Nothing)
	    (catchP (forever pgm)
	      (defaultStopHandler supC))
  where
    initPeerDB slots = PeerDB 2 weSeed slots M.empty []
    pgm = do chooseP [mgrEvent, infoEvent] >>= syncP
    mgrEvent = do
	  ev <- recvPC mgrCh
	  wrapP ev (\msg -> case msg of
			Tick                 -> tick
			RemovePeer t         -> removePeer t
			AddPeer t pCh -> do
			    logDebug $ "Adding peer " ++ show t
			    weSeed <- gets seeding
			    addPeer' pCh weSeed t)
    infoEvent = do
	  ev <- recvPC infoCh
	  wrapP ev (\m -> case m of
			    BlockComplete pn blk -> informBlockComplete pn blk
			    PieceDone pn -> informDone pn
			    TorrentComplete -> do
				modify (\s -> s { seeding = True
						, peerMap =
						   M.map (\pi -> pi { pAreSeeding = True })
						         $ peerMap s}))
    tick = do logDebug "Ticked"
	      ch <- asks mgrCh
	      TimerP.register 10 Tick ch
	      updateDB
	      runRechokeRound
    removePeer tid = do logDebug $ "Removing peer " ++ show tid
			modify (\db -> db { peerMap = M.delete tid (peerMap db) })

-- INTERNAL FUNCTIONS
----------------------------------------------------------------------

type PeerPid = ThreadId -- For now, should probably change


-- | The PeerDB is the database we keep over peers. It maps all the information necessary to determine
--   which peers are interesting to keep uploading to and which are slow. It also keeps track of how
--   far we are in the process of wandering the optimistic unchoke chain.
data PeerDB = PeerDB
    { chokeRound :: Int       -- ^ Counted down by one from 2. If 0 then we should advance the peer chain.
    , seeding :: Bool         -- ^ True if we are seeding the torrent. In a multi-torrent world, this has to change.
    , uploadSlots :: Int      -- ^ Current number of upload slots
    , peerMap :: PeerMap      -- ^ Map of peers
    , peerChain ::  [PeerPid] -- ^ The order in which peers are optimistically unchoked
    }

-- | The PeerInfo structure maps, for each peer pid, its accompanying informative data for the PeerDB
data PeerInfo = PeerInfo
      { pChokingUs :: Bool
      , pDownRate :: Double -- ^ The rate of the peer in question, bytes downloaded in last window
      , pUpRate   :: Double -- ^ The rate of the peer in question, bytes uploaded in last window
      , pChannel :: PeerChannel -- ^ The channel on which to communicate with the peer
      , pInterestedInUs :: Bool -- ^ Reflection from Peer DB
      , pAreSeeding :: Bool -- ^ True if this peer is connected on a torrent we seed
      , pIsASeeder :: Bool -- ^ True if the peer is a seeder
      }

type PeerMap = M.Map PeerPid PeerInfo

-- | Auxilliary data structure. Used in the rechoking process.
type RechokeData = (PeerPid, PeerInfo)

compareInv :: Ord a => a -> a -> Ordering
compareInv x y =
    case compare x y of
	LT -> GT
	EQ -> EQ
	GT -> LT

comparingWith :: Ord a => (a -> a -> Ordering) -> (b -> a) -> b -> b -> Ordering
comparingWith comp project x y =
    comp (project x) (project y)

-- | Leechers are sorted by their current download rate. We want to keep fast peers around.
sortLeech :: [RechokeData] -> [RechokeData]
sortLeech = sortBy (comparingWith compareInv $ pDownRate . snd)

-- | Seeders are sorted by their current upload rate.
sortSeeds :: [RechokeData] -> [RechokeData]
sortSeeds = sortBy (comparingWith compareInv $ pUpRate . snd)

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
addPeer' :: PeerChannel -> Bool -> PeerPid -> ChokeMgrProcess ()
addPeer' pCh weSeeding tid = do
    addPeerChain tid
    modify (\db -> db { peerMap = M.insert tid initialPeerInfo (peerMap db)})
  where
    initialPeerInfo = PeerInfo { pChokingUs = True
			       , pDownRate = 0.0
			       , pUpRate   = 0.0
			       , pChannel = pCh
			       , pInterestedInUs = False
			       , pAreSeeding = weSeeding -- TODO: Update this on torrent completion
			       , pIsASeeder = False -- May get updated quickly
			       }

-- | Insert a Peer randomly into the Peer chain. Threads the random number generator
--   through.
addPeerChain :: PeerPid -> ChokeMgrProcess ()
addPeerChain pid = do
    ls <- gets peerChain
    pt <- liftIO $ getStdRandom (\gen -> randomR (0, length ls - 1) gen)
    let (front, back) = splitAt pt ls
    modify (\db -> db { peerChain = (front ++ pid : back) })

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

-- | @selectPeers upSlots d s@ selects peers from a list of downloader peers @d@ and a list of seeder
--   peers @s@. The value of @upSlots@ defines the number of upload slots available
selectPeers :: Int -> [RechokeData] -> [RechokeData] -> S.Set PeerPid
selectPeers uploadSlots downPeers seedPeers = S.union downPids seedPids
    where
      (nDownSlots, nSeedSlots) = assignUploadSlots
                                 uploadSlots
                                 downPeers
                                 seedPeers
      downPids = S.fromList $ map fst $ take nDownSlots $ sortLeech downPeers
      seedPids = S.fromList $ map fst $ take nSeedSlots $ sortSeeds seedPeers

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


buildRechokeData :: ChokeMgrProcess [RechokeData]
buildRechokeData = do
    chain <- gets peerChain
    pm    <- gets peerMap
    return $ map (cPeer pm) chain
  where cPeer pm pid = (pid, fromJust $ M.lookup pid pm)

rechoke :: ChokeMgrProcess ()
rechoke = do
    peers <- buildRechokeData
    us <- gets uploadSlots
    let (down, seed) = splitSeedLeech peers
        electedPeers = selectPeers us down seed
    liftIO $ performChokingUnchoking electedPeers peers


informDone :: PieceNum -> ChokeMgrProcess ()
informDone pn = do
    T.mapM sendDone =<< gets peerMap
    return ()
  where
    sendDone pi = do
	st <- get
	c  <- ask
	(a, s') <- liftIO $ runP c st (proc pi) `catches`
	    [ Handler (\BlockedOnDeadMVar -> return ((), st)) ] -- Peer dead, ignore it
	put s'
	return a
    proc pi = do
	(sendP (pChannel pi) $ PieceCompleted pn) >>= syncP

informBlockComplete :: PieceNum -> Block -> ChokeMgrProcess ()
informBlockComplete pn blk = do
    T.mapM sendComp =<< gets peerMap
    return ()
  where
    sendComp pi = do
	st <- get
	c  <- ask
	(a, s') <- liftIO $ runP c st (proc pi) `catches`
	    [ Handler (\BlockedOnDeadMVar -> return ((), st)) ] -- Peer dead, ignore it
	put s'
	return a
    proc pi = do
	(sendP (pChannel pi) $ CancelBlock pn blk) >>= syncP

updateDB :: ChokeMgrProcess ()
updateDB = do
    nmp <- T.mapM gatherRate =<< gets peerMap
    modify (\db -> db { peerMap = nmp })
  where
      gatherRate pi = do
	ch <- liftIO $ channel
	-- The following should be refactored to the Process module
	st <- get
	c  <- ask
	(a, s') <- liftIO $ runP c st (proc ch pi) `catches`
	    [ Handler (\BlockedOnDeadMVar -> return (pi, st)) ] -- Peer Dead, ignore it
	put s'
	return a
      proc ch pi = do
        t <- liftIO getCurrentTime
	(sendP (pChannel pi) $ PeerStats t ch) >>= syncP
	(uprt, downrt, interested) <- recvP ch (const True) >>= syncP
	return pi { pDownRate = downrt,
		    pUpRate   = uprt,
	            pInterestedInUs = interested }

runRechokeRound :: ChokeMgrProcess ()
runRechokeRound = do
    cRound <- gets chokeRound
    if (cRound == 0)
	then do chain <- gets peerChain
		pm    <- gets peerMap
	        modify (\db -> db { chokeRound = 2,
				    peerChain = advancePeerChain chain pm })
	else modify (\db -> db { chokeRound = (chokeRound db) - 1 })
    rechoke
