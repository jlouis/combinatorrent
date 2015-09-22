{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}

-- | The following module is responsible for general types used
--   throughout the system.
module Torrent (
    -- * Types
      InfoHash
    , PeerId
    , AnnounceURL
    , TorrentState(..)
    , TorrentInfo(..)
    , PieceNum
    , PieceSize
    , PieceMap
    , PiecesDoneMap
    , PieceInfo(..)
    , BlockSize
    , Block(..)
    , Capabilities(..)
    -- * Interface
    , bytesLeft
    , defaultBlockSize
    , defaultOptimisticSlots
    , defaultPort
    , mkPeerId
    , mkTorrentInfo
    )
where

import Control.Applicative
import Control.DeepSeq

import Data.Array
import Data.List
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word
import GHC.Generics
import Numeric

import System.Random
import System.Random.Shuffle
import Test.QuickCheck

import Protocol.BCode
import Digest
import Version

-- | The type of Infohashes as used in torrents. These are identifiers
--   of torrents
type InfoHash = Digest

-- | The peerId is the ID of a client. It is used to identify clients
--   from each other
type PeerId   = String

-- | The internal type of Announce URLs
type AnnounceURL = B.ByteString

-- | Internal type for a torrent. It identifies a torrent in various places of the system.
data TorrentInfo = TorrentInfo {
      infoHash    :: InfoHash,
      pieceCount  :: Int, -- Number of pieces in torrent
      announceURLs :: [[AnnounceURL]]
      } deriving Show

data TorrentState = Seeding | Leeching
    deriving (Show, Generic)

instance NFData TorrentState

----------------------------------------------------------------------
-- Capabilities

data Capabilities = Fast | Extended
  deriving (Show, Eq)

-- PIECES
----------------------------------------------------------------------
type PieceNum = Int
type PieceSize = Int

data PieceInfo = PieceInfo {
      offset :: !Integer,     -- ^ Offset of the piece, might be greater than Int
      len ::    !Integer,     -- ^ Length of piece; usually a small value
      digest :: !B.ByteString -- ^ Digest of piece; taken from the .torret file
    } deriving (Eq, Show)

type PieceMap = Array PieceNum PieceInfo

-- | The PiecesDoneMap is a map which is true if we have the piece and false otherwise
type PiecesDoneMap = M.Map PieceNum Bool

-- | Return the amount of bytes left on a torrent given what pieces are done and the
--   map of the shape of the torrent in question.
bytesLeft :: PiecesDoneMap -> PieceMap -> Integer
bytesLeft done pm =
    foldl' (\accu (k,v) ->
        case M.lookup k done of
            Just False -> (len v) + accu
            _          -> accu) 0 $ Data.Array.assocs pm

-- BLOCKS
----------------------------------------------------------------------
type BlockSize = Int

data Block = Block { blockOffset :: !Int        -- ^ offset of this block within the piece
                   , blockSize   :: !BlockSize  -- ^ size of this block within the piece
                   } deriving (Eq, Ord, Show)

instance NFData Block where
    rnf (Block bo sz) = rnf bo `seq` rnf sz `seq` ()

instance Arbitrary Block where
  arbitrary = Block <$> pos <*> pos
    where pos = choose (0, 4294967296 - 1)


defaultBlockSize :: BlockSize
defaultBlockSize = 16384 -- Bytes

-- | Default number of optimistic slots
defaultOptimisticSlots :: Int
defaultOptimisticSlots = 2

-- | Default port to communicate on
defaultPort :: Word16
defaultPort = 1579

-- | Convert a BCode block into its corresponding TorrentInfo block, perhaps
--   failing in the process.
mkTorrentInfo :: BCode -> IO TorrentInfo
mkTorrentInfo bc = do
  (ann, np) <- case queryInfo bc of Nothing -> fail "Could not create torrent info"
                                    Just x -> return x
  ih  <- hashInfoDict bc
  let alist = fromMaybe [[ann]] $ announceList bc
  -- BEP012 says that lists of URL inside each tier must be shuffled
  gen <- newStdGen    
  let alist' = map (\xs -> shuffle' xs (length xs) gen) alist    
  return TorrentInfo { infoHash = ih, pieceCount = np, announceURLs = alist'}
    where
      queryInfo b =
        do ann <- announce b
           np  <- numberPieces b
           return (ann, np)

-- | Create a new PeerId for this client
mkPeerId :: StdGen -> PeerId
mkPeerId gen = header ++ take (20 - length header) ranString
  where randomList :: Int -> StdGen -> [Int]
        randomList n = take n . unfoldr (Just . random)
        rs = randomList 10 gen
        ranString = concatMap (\i -> showHex (abs i) "") rs
        header = "-CT" ++ protoVersion ++ "-"
