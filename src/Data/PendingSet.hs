module Data.PendingSet
    ( PendingSet
    , Data.PendingSet.empty
    , Data.PendingSet.size
    , have
    , unhave
    , haves
    , unhaves
    , pick
    )
where

import Data.PSQueue hiding (foldl)

import Torrent

-- | Representation of Pending Sets.
newtype PendingSet = PendingSet { unPS :: PSQ PieceNum Int }

-- | The empty pending set.
empty :: PendingSet
empty = PendingSet Data.PSQueue.empty

size :: PendingSet -> Int
size = Data.PSQueue.size . unPS

-- | A peer has a given piece. Reflect this in the PendingSet.
have :: PieceNum -> PendingSet -> PendingSet
have pn = PendingSet . alter f pn . unPS
  where f Nothing = Just 1
        f (Just x) = Just (x + 1)

-- | A Peer does not have a given piece anymore (TODO: Not used in practice)
unhave :: PieceNum -> PendingSet -> PendingSet
unhave pn = PendingSet . alter f pn . unPS
  where f Nothing  =  error "Data.PendingSet.unhave"
        f (Just 1) = Nothing
        f (Just x) = Just (x-1)

-- | Add all pieces in a bitfield
haves :: [PieceNum] -> PendingSet -> PendingSet
haves pns = flip (foldl f) pns
  where f e = flip have e

-- | Remove all pieces in a bitfield
unhaves :: [PieceNum] -> PendingSet -> PendingSet
unhaves pns = flip (foldl f) pns
  where f e = flip unhave e

-- | Crawl through the set of pending pieces in decreasing order of rarity.
-- Each piece is discriminated by a selector function until the first hit is
-- found. Then all Pieces of the same priority accepted by the selector is
-- chosen for return.
pick :: (PieceNum -> Bool) -> PendingSet -> Maybe [PieceNum]
pick selector ps = findPri (minView . unPS $ ps)
  where findPri Nothing = Nothing
        findPri (Just (pn :-> p, rest)) =
            if selector pn
                then pickAtPri p [pn] (minView rest)
                else findPri $ minView rest
        pickAtPri _p acc Nothing = Just acc
        pickAtPri  p acc (Just (pn :-> p', rest))
            | p == p' = if selector pn
                            then pickAtPri p (pn : acc) $ minView rest
                            else pickAtPri p acc $ minView rest
            | otherwise = Just acc

