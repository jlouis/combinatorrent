{-# LANGUAGE FlexibleContexts #-}
module Process.ChokeMgr (
    -- * Types, Channels
      ChokeMgrChannel
    , ChokeMgrMsg(..)
    -- * Interface
    , start
    )
where

import Data.Time.Clock
import Data.List
import qualified Data.Set as S
import Data.Traversable as T

import Control.Concurrent
import Control.Concurrent.CML.Strict
import Control.DeepSeq
import Control.Exception (assert)
import Control.Monad.Reader
import Control.Monad.State


import Prelude hiding (catch, log)

import System.Random

import PeerTypes hiding (Peer)
import Process
import Supervisor
import Torrent hiding (infoHash)
import Process.Timer as Timer

-- DATA STRUCTURES
----------------------------------------------------------------------

-- | Messages to the Choke Manager
data ChokeMgrMsg = Tick                                  -- ^ Request that we run another round
                 | RemovePeer ThreadId                   -- ^ Request that this peer is removed
                 | AddPeer InfoHash ThreadId PeerChannel -- ^ Request that this peer is added
                 | PieceDone InfoHash PieceNum           -- ^ Note that a given piece is done
                 | BlockComplete InfoHash PieceNum Block -- ^ Note that a block is complete (endgame)
                 | TorrentComplete InfoHash              -- ^ Note that the torrent in question is complete

instance NFData ChokeMgrMsg where
  rnf a = a `seq` ()

type ChokeMgrChannel = Channel ChokeMgrMsg

data CF = CF { mgrCh :: ChokeMgrChannel }

instance Logging CF where
  logName _ = "Process.ChokeMgr"

-- PeerDB described below
type ChokeMgrProcess a = Process CF PeerDB a

-- INTERFACE
----------------------------------------------------------------------

start :: ChokeMgrChannel -> Int -> Bool -> SupervisorChan
      -> IO ThreadId
start ch ur weSeed supC = do
    Timer.register 10 Tick ch
    spawnP (CF ch) (initPeerDB $ calcUploadSlots ur Nothing)
            (catchP (forever pgm)
              (defaultStopHandler supC))
  where
    initPeerDB slots = PeerDB 2 slots S.empty []
    pgm = {-# SCC "ChokeMgr" #-} mgrEvent >>= syncP
    mgrEvent =
          recvWrapPC mgrCh
            (\msg ->
                case msg of
                    Tick          -> tick
                    RemovePeer t  -> removePeer t
                    AddPeer ih t pCh -> do
                            debugP $ "Adding peer " ++ show (ih, t)
                            addPeer pCh ih t
                    BlockComplete ih pn blk -> informBlockComplete ih pn blk
                    PieceDone ih pn -> informDone ih pn
                    TorrentComplete ih -> modify (\s -> s { seeding = S.insert ih $ seeding s }))
    tick = do debugP "Ticked"
              ch <- asks mgrCh
              Timer.register 10 Tick ch
              updateDB
              runRechokeRound
    removePeer tid = do debugP $ "Removing peer " ++ show tid
                        modify (\db -> db { chain = filter (not . isPeer tid) (chain db) })
    isPeer tid pr | tid == pThreadId pr = True
                  | otherwise           = False

-- INTERNAL FUNCTIONS
----------------------------------------------------------------------

-- The data structure is split into pieces so it is easier to manipulate.
-- The PeerDB is the state we thread around in the process. The PChain contains all
-- the important information about processes.
type PChain = [Peer]

-- | Main data for a peer
data Peer   = Peer
        { pThreadId :: ThreadId
        , pInfoHash :: InfoHash
        , pChannel  :: PeerChannel
        , pRate     :: PRate
        , pState    :: PState
        }

-- | Peer upload and download ratio
data PRate  = PRate { pUpRate   :: Double,
                      pDownRate :: Double }
-- | Current State of the peer
data PState = PState { pChokingUs :: Bool -- ^ True if the peer is choking us
                     , pInterestedInUs :: Bool -- ^ Reflection from Peer DB
                     , pIsASeeder :: Bool -- ^ True if the peer is a seeder
                     }
data PeerDB = PeerDB
    { chokeRound  :: Int      -- ^ Counted down by one from 2. If 0 then we should
                              --   advance the peer chain. (Optimistic Unchoking)
    , uploadSlots :: Int      -- ^ Current number of upload slots
    , seeding     :: S.Set InfoHash -- ^ Set of torrents we seed
    , chain       :: PChain   -- ^ The order in which peers are optimistically unchoked
    }

-- | Traverse all peers and process them with an operation.
traversePeers :: (MonadState PeerDB m) => (Peer -> m Peer) -> m ()
traversePeers operation = do
    nPeers <- T.mapM operation =<< gets chain
    modify (\db -> db { chain = nPeers })

-- | Update the Peer Database with the newest information from peers
--   TODO: Can benefit from some CML love.
updateDB :: ChokeMgrProcess ()
updateDB = traversePeers gatherRate
  where
      gatherRate pi = do
        ch <- liftIO channel
        t  <- liftIO getCurrentTime
        ignoreProcessBlock pi (gather t ch pi)
      gather t ch pi = do
        (sendP (pChannel pi) $ PeerStats t ch) >>= syncP
        (uprt, downrt, interested) <- recvP ch (const True) >>= syncP
        let rt = PRate { pUpRate = uprt, pDownRate = downrt }
            st = (pState pi) { pInterestedInUs = interested }
        return $ pi { pRate = rt, pState = st }

addPeer :: PeerChannel -> InfoHash -> ThreadId -> ChokeMgrProcess ()
addPeer ch ih t = do
    chn <- gets chain
    pt  <- liftIO $ getStdRandom (\gen -> randomR (0, length chn - 1) gen)
    let (front, back) = splitAt pt chn
    modify (\db -> db { chain = (front ++ initPeer : back) })
  where initPeer = Peer t ih ch initRate initState
        initRate = PRate 0.0 0.0
        initState = PState True False False

runRechokeRound :: ChokeMgrProcess ()
runRechokeRound = do
    cRound <- gets chokeRound
    if (cRound == 0)
        then do nChain <- advancePeerChain
                modify (\db -> db { chokeRound = 2 })
        else modify (\db -> db { chokeRound = (chokeRound db) - 1 })
    rechoke

-- | Advance the peer chain to the next peer eligible for optimistic
--   unchoking. That is, skip peers which are not interested in our pieces
--   and peers which are not choking us. The former we can't send any data to,
--   so we can't get better speeds at them. The latter are already sending us data,
--   so we know how good they are as peers.
advancePeerChain :: ChokeMgrProcess ()
advancePeerChain = do
    peers <- gets chain
    let (front, back) = break breakPoint peers
    modify (\db -> db { chain = back ++ front })
  where
    breakPoint peer =
        let st = pState peer
        in pInterestedInUs st && pChokingUs st

rechoke :: ChokeMgrProcess ()
rechoke = do
    us <- gets uploadSlots
    chn <- gets chain
    sd <- gets seeding
    let (seed, down) = splitSeedLeech sd chn
    electedPeers <- selectPeers us down seed
    performChokingUnchoking electedPeers chn

-- | Function to split peers into those where we are seeding and those where we are leeching.
--   also prunes the list for peers which are not interesting.
--   TODO: Snubbed peers
splitSeedLeech :: S.Set InfoHash -> [Peer] -> ([Peer], [Peer])
splitSeedLeech seeders ps = partition (\p -> S.member (pInfoHash p) seeders) $ filter picker ps
  where
    -- TODO: pIsASeeder is always false at the moment
    picker :: Peer -> Bool
    picker p = not (pIsASeeder $ pState p) && (pInterestedInUs $ pState p)

-- | Comparison with inverse ordering
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
sortLeech :: [Peer] -> [Peer]
sortLeech = sortBy (comparingWith compareInv $ pDownRate . pRate)

-- | Seeders are sorted by their current upload rate.
sortSeeds :: [Peer] -> [Peer]
sortSeeds = sortBy (comparingWith compareInv $ pUpRate . pRate)


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
                             | otherwise = calcRate $ fromIntegral rate
  where calcRate :: Double -> Int
        calcRate x = round $ sqrt (x * 0.6)

-- | The call @assignUploadSlots c ds ss@ will assume that we have @c@
--   slots for uploading at our disposal. The list @ds@ will be peers
--   that we would like to upload to among the torrents we are
--   currently downloading. The list @ss@ is the same thing but for
--   torrents that we seed. The function returns a pair @(kd,ks)@
--   where @kd@ is the number of downloader slots and @ks@ is the
--   number of seeder slots.
--
--   The function will move surplus slots around so all of them gets used.
assignUploadSlots :: Int -> [Peer] -> [Peer] -> (Int, Int)
assignUploadSlots slots downloaderPeers seederPeers =
    -- Shuffle surplus slots around so all gets used
    shuffleSeeders . shuffleDownloaders $ (downloaderSlots, seederSlots)
  where
    -- Calculate the slots available for the downloaders and seeders
    --   We allocate 70% of them to leeching and 30% of the to seeding
    --   though we assign at least one slot to both
    slotRound :: Double -> Double -> Int
    slotRound slots fraction = max 1 $ round $ slots * fraction

    downloaderSlots = slotRound (fromIntegral slots) 0.7
    seederSlots     = slotRound (fromIntegral slots) 0.3

    -- Calculate the amount of peers wanting to download and seed
    numDownPeers = length downloaderPeers
    numSeedPeers = length seederPeers

    -- If there is a surplus of downloader slots, then assign them to
    --  the seeder slots
    shuffleDownloaders (dSlots, sSlots) =
        case max 0 (dSlots - numDownPeers) of
          0 -> (dSlots, sSlots)
          k -> (dSlots - k, sSlots + k)

    -- If there is a surplus of seeder slots, then assign these to
    --   the downloader slots. Limit the downloader slots to the number
    --   of downloaders, however
    shuffleSeeders (dSlots, sSlots) =
        case max 0 (sSlots - numSeedPeers) of
          0 -> (dSlots, sSlots)
          k -> (min (dSlots + k) numDownPeers, sSlots - k)

-- | @selectPeers upSlots d s@ selects peers from a list of downloader peers @d@ and a list of seeder
--   peers @s@. The value of @upSlots@ defines the number of upload slots available
selectPeers :: Int -> [Peer] -> [Peer] -> ChokeMgrProcess (S.Set ThreadId)
selectPeers uploadSlots downPeers seedPeers = do
        -- Construct a set of downloaders (leechers) and a Set of seeders, which have the
        --  current best rates
        let (nDownSlots, nSeedSlots) = assignUploadSlots uploadSlots downPeers seedPeers
            downPids = S.fromList $ map pThreadId $ take nDownSlots $ sortLeech downPeers
            seedPids = S.fromList $ map pThreadId $ take nSeedSlots $ sortSeeds seedPeers
        debugP $ "Slots: " ++ show nDownSlots ++ " downloads, " ++ show nSeedSlots ++ " seeders"
        debugP $ "Electing peers - leechers: " ++ show downPids ++ "; seeders: " ++ show seedPids
        return $ assertSlots (nDownSlots + nSeedSlots) (S.union downPids seedPids)
  where assertSlots slots = assert (uploadSlots >= slots)

-- | Send a message to the peer process at PeerChannel. Message is sent asynchronously
--   to the peer in question. If the system is really loaded, this might
--   actually fail since the order in which messages arrive might be inverted.
msgPeer :: PeerChannel -> PeerMessage -> ChokeMgrProcess ThreadId
msgPeer ch = liftIO . spawn . sync . (transmit ch)

-- | This function performs the choking and unchoking of peers in a round.
performChokingUnchoking :: S.Set ThreadId -> [Peer] -> ChokeMgrProcess ()
performChokingUnchoking elected peers =
    do T.mapM unchoke electedPeers
       optChoke defaultOptimisticSlots nonElectedPeers
  where
    -- Partition the peers in elected and non-elected
    (electedPeers, nonElectedPeers) = partition (\rd -> S.member (pThreadId rd) elected) peers
    unchoke p = msgPeer (pChannel p) UnchokePeer
    choke   p = msgPeer (pChannel p) ChokePeer

    -- If we have k optimistic slots, @optChoke k peers@ will unchoke the first
    -- @k@ peers interested in us. The rest will either be unchoked if they are
    -- not interested (ensuring fast start should they become interested); or
    -- they will be choked to avoid TCP/IP congestion.
    optChoke _ [] = return ()
    optChoke 0 (p : ps) = if (pInterestedInUs . pState) p
                               then choke p >> optChoke 0 ps
                               else unchoke p >> optChoke 0 ps
    optChoke k (p : ps) = if (pInterestedInUs . pState) p
                                then unchoke p >> optChoke (k-1) ps
                                else unchoke p >> optChoke k ps

informDone :: InfoHash -> PieceNum -> ChokeMgrProcess ()
informDone ih pn = do
    chn <- gets chain
    T.mapM inform chn >> return ()
 where inform p | (pInfoHash p) == ih = sendDone p >> return ()
                | otherwise           = return ()
       sendDone p = msgPeer (pChannel p) (PieceCompleted pn)

informBlockComplete :: InfoHash -> PieceNum -> Block -> ChokeMgrProcess ()
informBlockComplete ih pn blk = do
    chn <- gets chain
    T.mapM inform chn >> return ()
  where inform p | (pInfoHash p) == ih = sendComp p >> return ()
                 | otherwise           = return ()
        sendComp pi = msgPeer (pChannel pi) (CancelBlock pn blk)

