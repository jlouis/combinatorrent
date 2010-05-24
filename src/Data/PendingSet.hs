module Data.PendingSet
    ( PendingSet
    , Data.PendingSet.empty
    , Data.PendingSet.size
    , remove
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
  where f Nothing  =  Nothing
        f (Just 1) = Nothing
        f (Just x) = Just (x-1)

-- | Remove a piece from the histogram. Used when it completes
remove :: PieceNum -> PendingSet -> PendingSet
remove pn = PendingSet . delete pn . unPS

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
pick :: (PieceNum -> IO Bool) -> PendingSet -> IO (Maybe [PieceNum])
pick selector ps = findPri (minView . unPS $ ps)
  where findPri Nothing = return Nothing
        findPri (Just (pn :-> p, rest)) = do
            r <- selector pn
            if r
                then pickAtPri numToPick p [pn] (minView rest)
                else findPri $ minView rest
        pickAtPri 0 _p acc _ = return $ Just acc
        pickAtPri _ _p acc Nothing = return $ Just acc
        pickAtPri k p acc (Just (pn :-> p', rest))
            | p == p' = do
                r <- selector pn
                if r
                    then pickAtPri (k-1) p (pn : acc) $ minView rest
                    else pickAtPri k p acc $ minView rest
            | otherwise = return $ Just acc

-- | Number of pieces to pick with the picker. Setting an upper limit here because if a lot
--   of peers have all pieces, these numbers grow insanely big, leading to allocation we
--   don't really need.
numToPick :: Int
numToPick = 7
