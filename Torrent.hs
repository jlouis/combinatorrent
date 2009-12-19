-- Haskell Torrent
-- Copyright (c) 2009, Jesper Louis Andersen,
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--  * Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-- | The following module is responsible for general types used
--   throughout the system.
module Torrent (InfoHash,
                PeerId,
                AnnounceURL,
                TorrentInfo(..),
                PieceNum,
                MissingMap,
                PieceInfo(..),
                haskellTorrentPort,
                haskellTorrentVersion,
                mkPeerId,
                mkTorrentInfo)
where

import qualified Data.ByteString.Lazy as B
import Data.List
import qualified Data.Map as M

import Network
import Numeric

import System.Random

import BCode

-- | The type of Infohashes as used in torrents. These are identifiers
--   of torrents
type InfoHash = B.ByteString

-- | The peerId is the ID of a client. It is used to identify clients
--   from each other
type PeerId   = String

-- | The internal type of Announce URLs
type AnnounceURL = String

-- | Internal type for a torrent. It identifies a torrent in various places of the system.
data TorrentInfo = TorrentInfo {
      infoHash :: InfoHash,
      pieceCount :: Integer, -- Number of pieces in torrent
      announceURL :: AnnounceURL } deriving Show

type PieceNum = Integer

data PieceInfo = PieceInfo {
      offset :: Integer,
      len :: Integer,
      digest :: B.ByteString } deriving (Eq, Show)

type MissingMap = M.Map Integer Bool

-- | Default port to communicate on
haskellTorrentPort :: PortID
haskellTorrentPort = PortNumber $ fromInteger 1579

-- | The current version of Haskell-Torrent. It should be be here.
haskellTorrentVersion :: String
haskellTorrentVersion = "d001"

-- | Convert a BCode block into its corresponding TorrentInfo block, perhaps
--   failing in the process.
mkTorrentInfo :: BCode -> Maybe TorrentInfo
mkTorrentInfo bc =
    do ann <- announce bc
       ih  <- hashInfoDict bc
       np  <- numberPieces bc
       return TorrentInfo { infoHash = ih, announceURL = ann, pieceCount = np }

-- | Create a new PeerId for this client
mkPeerId :: StdGen -> PeerId
mkPeerId gen = header ++ (take (20 - length header) ranString)
  where randomList :: Int -> StdGen -> [Int]
        randomList n = take n . unfoldr (Just . random)
        rs = randomList 10 gen
        ranString = concatMap (\i -> showHex (abs i) "") rs
        header = "-HT" ++ haskellTorrentVersion ++ "-"
