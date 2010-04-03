{-# LANGUAGE FlexibleContexts, TupleSections #-}
module Process.ChokeMgr (
    -- * Types, Channels
      ChokeMgrChannel
    , RateTVar
    , ChokeMgrMsg(..)
    -- * Interface
    , start
    )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception (assert)
import Control.Monad.Reader
import Control.Monad.State

import Data.Function
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Traversable as T

import Prelude hiding (catch, log)

import System.Random

import Channels hiding (Peer)
import Process
import Supervisor
import Torrent hiding (infoHash)

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

type ChokeMgrChannel = TChan ChokeMgrMsg
type RateTVar = TVar [(ThreadId, (Double, Double, Bool, Bool, Bool))]

data CF = CF { mgrCh :: ChokeMgrChannel
             , rateTV :: RateTVar }

instance Logging CF where
  logName _ = "Process.ChokeMgr"

-- PeerDB described below
type ChokeMgrProcess a = Process CF PeerDB a

-- INTERFACE
----------------------------------------------------------------------

roundTickSecs :: Int
roundTickSecs = 11

start :: ChokeMgrChannel -> RateTVar -> Int -> SupervisorChan
      -> IO ThreadId
start ch rtv ur supC = do
    _ <- registerTimer ch
    spawnP (CF ch rtv) (initPeerDB $ calcUploadSlots ur Nothing)
            (catchP (forever pgm)
              (defaultStopHandler supC))
  where
    initPeerDB slots = PeerDB 2 slots S.empty M.empty []
    pgm = do
        msg <- liftIO . atomically $ readTChan ch
        case msg of
           Tick          -> tick
           RemovePeer t  -> removePeer t
           AddPeer ih t pCh -> do
                   debugP $ "Adding peer " ++ show (ih, t)
                   addPeer pCh ih t
           BlockComplete ih pn blk -> informBlockComplete ih pn blk
           PieceDone ih pn -> informDone ih pn
           TorrentComplete ih -> modify (\s -> s { seeding = S.insert ih $ seeding s })
    tick = do debugP "Ticked"
              c <- asks mgrCh
              _ <- liftIO (registerTimer c)
              updateDB
              runRechokeRound
    removePeer tid = do debugP $ "Removing peer " ++ show tid
                        modify (\db -> db { chain = filter (not . isPeer tid) (chain db)
                                          , rateMap = M.delete tid (rateMap db) })
    isPeer tid pr | tid == pThreadId pr = True
                  | otherwise           = False

registerTimer :: ChokeMgrChannel -> IO ThreadId
registerTimer c = do
    forkIO $ do threadDelay (roundTickSecs * 1000000)
                atomically $ writeTChan c Tick

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
        }

-- | Peer upload and download ratio
data PRate  = PRate { pUpRate   :: Double,
                      pDownRate :: Double }
-- | Current State of the peer
data PState = PState { pChokingUs :: Bool -- ^ True if the peer is choking us
                     , pInterestedInUs :: Bool -- ^ Reflection from Peer DB
                     , pIsASeeder :: Bool -- ^ True if the peer is a seeder
                     }

type RateMap = M.Map ThreadId (PRate, PState)
data PeerDB = PeerDB
    { chokeRound  :: Int      -- ^ Counted down by one from 2. If 0 then we should
                              --   advance the peer chain. (Optimistic Unchoking)
    , uploadSlots :: Int      -- ^ Current number of upload slots
    , seeding     :: S.Set InfoHash -- ^ Set of torrents we seed
    , rateMap    :: RateMap  -- ^ Map from Peer ThreadIds to state
    , chain       :: PChain   -- ^ The order in which peers are optimistically unchoked
    }

-- | Update the Peer Database with the newest information from peers
--   TODO: Can benefit from some CML love.
updateDB :: ChokeMgrProcess ()
updateDB = do
    rc <- asks rateTV
    rateUpdate <- liftIO . atomically $ do
                    q <- readTVar rc
                    writeTVar rc []
                    return q
    case rateUpdate of
        [] -> return ()
        updates ->  let f old (tid, (uprt, downrt, interested, seeder, choking)) =
                             M.insert tid (PRate { pUpRate = uprt, pDownRate = downrt },
                                           PState { pInterestedInUs = interested,
                                                    pIsASeeder      = seeder,
                                                    pChokingUs      = choking }) old
                        nm m = foldl f m $ reverse updates
                    in do
                        debugP $ "Rate updates since last round: " ++ show updates
                        modify (\db -> db { rateMap = nm (rateMap db) })

addPeer :: PeerChannel -> InfoHash -> ThreadId -> ChokeMgrProcess ()
addPeer ch ih t = do
    chn <- gets chain
    pt  <- liftIO $ getStdRandom (\gen -> randomR (0, length chn - 1) gen)
    let (front, back) = splitAt pt chn
    modify (\db -> db { chain = (front ++ initPeer : back) })
  where initPeer = Peer t ih ch

runRechokeRound :: ChokeMgrProcess ()
runRechokeRound = do
    cRound <- gets chokeRound
    if (cRound == 0)
        then do advancePeerChain
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
    rd <- gets rateMap
    let (front, back) = break (breakPoint rd) peers
    modify (\db -> db { chain = back ++ front })
  where
    breakPoint rd peer =
        case M.lookup (pThreadId peer) rd of
            Nothing -> True -- Really new peer, give it the chance :)
            Just (_, st) -> pInterestedInUs st && pChokingUs st

rechoke :: ChokeMgrProcess ()
rechoke = do
    us <- gets uploadSlots
    chn <- gets chain
    sd <- gets seeding
    rm <- gets rateMap
    debugP $ "Chain is:  " ++ show (map pThreadId chn)
    let (seed, down) = splitSeedLeech sd rm chn
    electedPeers <- selectPeers us down seed
    performChokingUnchoking electedPeers chn

-- | Function to split peers into those where we are seeding and those where we are leeching.
--   also prunes the list for peers which are not interesting.
--   TODO: Snubbed peers
splitSeedLeech :: S.Set InfoHash -> RateMap -> [Peer] -> ([Peer], [Peer])
splitSeedLeech seeders rm ps = partition (\p -> S.member (pInfoHash p) seeders) $ filter picker ps
  where
    -- TODO: pIsASeeder is always false at the moment
    picker :: Peer -> Bool
    picker p = case M.lookup (pThreadId p) rm of
                 Nothing -> False -- Don't know anything about the peer yet
                 Just (_, st) -> not (pIsASeeder st) && (pInterestedInUs st)

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
sortLeech :: [(Peer, (PRate, PState))] -> [(Peer, (PRate, PState))]
sortLeech = sortBy $ comparingWith compareInv (pDownRate . fst . snd)

-- | Seeders are sorted by their current upload rate.
sortSeeds :: [(Peer, (PRate, PState))] -> [(Peer, (PRate, PState))]
sortSeeds = sortBy $ comparingWith compareInv (pUpRate . fst . snd)


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
assignUploadSlots :: Int -> Int -> Int -> (Int, Int)
assignUploadSlots slots numDownPeers numSeedPeers =
    -- Shuffle surplus slots around so all gets used
    shuffleSeeders . shuffleDownloaders $ (downloaderSlots, seederSlots)
  where
    -- Calculate the slots available for the downloaders and seeders
    --   We allocate 70% of them to leeching and 30% of the to seeding
    --   though we assign at least one slot to both
    slotRound :: Double -> Double -> Int
    slotRound ss fraction = max 1 $ round $ ss * fraction

    downloaderSlots = slotRound (fromIntegral slots) 0.7
    seederSlots     = slotRound (fromIntegral slots) 0.3

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
selectPeers ups downPeers seedPeers = do
        rm <- gets rateMap
        let selector p = maybe (p, (PRate 0.0 0.0, PState True False False)) (p,)
                            (M.lookup (pThreadId p) rm)
            dp = map selector downPeers
            sp = map selector seedPeers
            (nDownSlots, nSeedSlots) = assignUploadSlots ups (length downPeers) (length seedPeers)
            downPids = S.fromList $ map (pThreadId . fst) $ take nDownSlots $ sortLeech dp
            seedPids = S.fromList $ map (pThreadId . fst) $ take nSeedSlots $ sortSeeds sp
        debugP $ "Leechers: " ++ show (length downPeers) ++ ", Seeders: " ++ show (length seedPeers)
        debugP $ "Slots: " ++ show nDownSlots ++ " downloads, " ++ show nSeedSlots ++ " seeders"
        debugP $ "Electing peers - leechers: " ++ show downPids ++ "; seeders: " ++ show seedPids
        return $ assertSlots (nDownSlots + nSeedSlots) (S.union downPids seedPids)
  where assertSlots slots = assert (ups >= slots)

-- | Send a message to the peer process at PeerChannel. Message is sent asynchronously
--   to the peer in question. If the system is really loaded, this might
--   actually fail since the order in which messages arrive might be inverted.
msgPeer :: PeerChannel -> PeerMessage -> ChokeMgrProcess ()
msgPeer ch = liftIO . atomically . writeTChan ch

-- | This function performs the choking and unchoking of peers in a round.
performChokingUnchoking :: S.Set ThreadId -> [Peer] -> ChokeMgrProcess ()
performChokingUnchoking elected peers =
    do _ <- T.mapM unchoke electedPeers
       rm <- gets rateMap
       optChoke rm defaultOptimisticSlots nonElectedPeers
  where
    -- Partition the peers in elected and non-elected
    (electedPeers, nonElectedPeers) = partition (\rd -> S.member (pThreadId rd) elected) peers
    unchoke p = msgPeer (pChannel p) UnchokePeer
    choke   p = msgPeer (pChannel p) ChokePeer

    -- If we have k optimistic slots, @optChoke k peers@ will unchoke the first
    -- @k@ peers interested in us. The rest will either be unchoked if they are
    -- not interested (ensuring fast start should they become interested); or
    -- they will be choked to avoid TCP/IP congestion.
    optChoke _rm _ [] = return ()
    optChoke  rm 0 (p : ps) =
        case M.lookup (pThreadId p) rm of
            Nothing -> choke p >> optChoke rm 0 ps
            Just (_, st) ->
                if pInterestedInUs st
                   then choke p >> optChoke rm 0 ps
                   else unchoke p >> optChoke rm 0 ps
    optChoke rm k (p : ps) =
        case M.lookup (pThreadId p) rm of
            Nothing -> unchoke p >> optChoke rm (k-1) ps
            Just (_, st) ->
                if pInterestedInUs st
                   then unchoke p >> optChoke rm (k-1) ps
                   else unchoke p >> optChoke rm k ps

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
        sendComp p = msgPeer (pChannel p) (CancelBlock pn blk)

