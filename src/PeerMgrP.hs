module PeerMgrP (Peer(..),
                 start)
where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S

import Control.Concurrent
import Control.Concurrent.CML

import System.Random

import PeerP
import PeerTypes
import PieceMgrP hiding (start)
import ConsoleP hiding (start)
import FSP hiding (start)
import Torrent hiding (infoHash)


data State = MkState { peerCh :: Channel [Peer],
                       pieceMgrCh :: PieceMgrChannel,
                       peersInQueue  :: [Peer],
                       mgrCh :: Channel MgrMessage,
                       peers :: M.Map ThreadId (Channel PeerMessage),
                       peerId :: PeerId,
                       infoHash :: InfoHash,
                       fsCh :: FSPChannel,
                       logCh :: LogChannel }

start :: Channel [Peer] -> PeerId -> InfoHash -> PieceMap -> PieceMgrChannel -> FSPChannel -> LogChannel -> Int -> IO ()
start ch pid ih pm pieceMgrC fsC logC nPieces =
    do mgrC <- channel
       spawn $ lp (MkState ch pieceMgrC [] mgrC M.empty pid ih fsC logC )
       return ()
  where lp s = do logMsg logC "Looping PeerMgr"
                  sync (choose [trackerPeers s, peerEvent s]) >>= fillPeers >>= lp
        trackerPeers s = wrap (receive (peerCh s) (const True))
                           (\ps ->
                                do logMsg (logCh s) "Adding peers to queue"
                                   return s { peersInQueue = ps ++ peersInQueue s })
        peerEvent s = wrap (receive (mgrCh s) (const True))
                        (\msg ->
                             case msg of
                               Connect tid c -> newPeer s tid c
                               Disconnect tid -> removePeer s tid)
        newPeer s tid c  = do logMsg (logCh s) "Unchoking new peer"
                              sync $ transmit c UnchokePeer -- TODO: This is a hack for now
                              return s { peers = M.insert tid c (peers s)}
        removePeer s tid = do logMsg (logCh s) "Deleting peer"
                              return s { peers = M.delete tid (peers s) }
        fillPeers s | M.size (peers s) > 40 = return s
                    | otherwise =
                        do let (toAdd, rest) = splitAt (40 - M.size (peers s)) (peersInQueue s)
                           logMsg (logCh s) $ "Filling with up to " ++ show (40 - M.size (peers s)) ++ " peers"
                           mapM_ (addPeer s) toAdd
                           return s { peersInQueue = rest }
        addPeer s (Peer hn prt) = do
          logMsg (logCh s) "Adding peer"
          PeerP.connect hn prt (peerId s) (infoHash s) pm (pieceMgrCh s) (fsCh s) (logCh s) (mgrCh s) nPieces
          logMsg (logCh s) "... Added"

-- INTERNAL FUNCTIONS
----------------------------------------------------------------------

type PeerPid = Int -- For now, should probably change

data PeerInfo = PeerInfo
      { chokingUs :: Bool
      , interestedInUs :: Bool
      }

type PeerMap = M.Map PeerPid PeerInfo

data RechokeData = RechokeData
    { rdPeerPid :: PeerPid -- ^ The Pid of the peer
    , rdDownRate :: Double -- ^ The rate of the peer in question, bytes downloaded in last window
    , rdChannel :: PeerChannel -- ^ The channel on which to communicate with the peer
    , rdInterestedInUs :: Bool -- ^ Reflection from Peer DB
    }

-- | Peers are sorted by their current download rate. We want to keep fast peers around.
sortPeers :: [RechokeData] -> [RechokeData]
sortPeers = sortBy (comparing rdDownRate)

-- | Advance the peer chain to the next peer eligible for optimistic
--   unchoking. That is, skip peers which are not interested in our pieces
--   and peers which are not choking us. The former we can't send any data to,
--   so we can't get better speeds at them. The latter are already sending us data,
--   so we know how good they are as peers.
advancePeerChain :: [PeerPid] -> PeerMap -> [PeerPid]
advancePeerChain [] mp = []
advancePeerChain peers mp = back ++ front
  where (front, back) = break (\p -> isInterested p mp && isChokingUs p mp) peers

-- | Insert a Peer randomly into the Peer chain. Threads the random number generator
--   through.
addPeerChain :: StdGen -> PeerPid -> [PeerPid] -> ([PeerPid], StdGen)
addPeerChain gen pid ls = (front ++ pid : back, gen')
  where (front, back) = splitAt pt ls
        (pt, gen') = randomR (0, length ls - 1) gen

-- | Predicate. Is the peer interested in any of our pieces?
isInterested :: PeerPid -> PeerMap -> Bool
isInterested p = interestedInUs . fromJust . (M.lookup p)

-- | Predicate. Is the peer choking us?
isChokingUs :: PeerPid -> PeerMap -> Bool
isChokingUs p = chokingUs . fromJust . (M.lookup p)

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
selectPeers uploadRate downPeers seedPeers = S.union (S.fromList $ take nDownSlots downPeers)
                                                     (S.fromList $ take nUpSlots seedPeers)
    where
      (nDownSlots, nUpSlots) = assignUploadSlots
                                 (calcUploadSlots uploadRate Nothing)
                                 downPeers
                                 seedPeers

-- | This function carries out the choking and unchoking of peers in a round.
performChokingUnchoking :: S.Set PeerPid -> [RechokeData] -> IO ()
performChokingUnchoking elected peers =
    do mapM_ unchoke unchokers
       optChoke defaultOptimisticSlots chokers
  where
    -- Partition the peers based on they were selected or not
    (unchokers, chokers) = partition (\rd -> S.member (rdPeerPid rd) elected) peers
    -- Unchoke peer p
    unchoke p = unchokePeer (rdChannel p)
    -- If we have k optimistic slots, @optChoke k peers@ will unchoke the first @k@ interested
    --  in us and choke the rest.
    optChoke _ [] = return ()
    optChoke 0 (p : ps) = chokePeer (rdChannel p) >> optChoke 0 ps
    optChoke k (p : ps) = unchokePeer (rdChannel p) >> if rdInterestedInUs p
                                                           then optChoke (k-1) ps
                                                           else optChoke k ps -- This check may be irrelevant
