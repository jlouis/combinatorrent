module Data.PendingSet
    ( Data.PendingSet.empty
    , have
    , unhave
    , bitfield
    , unbitfield
    , pick
    )
where

import Data.PSQueue hiding (foldl)

import Torrent

newtype PendingSet = PendingSet { unPS :: PSQ PieceNum Int }

empty :: PendingSet
empty = PendingSet Data.PSQueue.empty

have :: PieceNum -> PendingSet -> PendingSet
have pn = PendingSet . alter f pn . unPS
  where f Nothing = Just 1
        f (Just x) = Just (x + 1)

unhave :: PieceNum -> PendingSet -> PendingSet
unhave pn = PendingSet . alter f pn . unPS
  where f Nothing  =  error "Data.PendingSet.unhave"
        f (Just 1) = Nothing
        f (Just x) = Just (x-1)

bitfield :: [PieceNum] -> PendingSet -> PendingSet
bitfield pns = flip (foldl f) pns
  where f e = flip have e

unbitfield :: [PieceNum] -> PendingSet -> PendingSet
unbitfield pns = flip (foldl f) pns
  where f e = flip unhave e

pick :: (PieceNum -> Bool) -> PendingSet -> Maybe [PieceNum]
pick selector ps = findPri (minView . unPS $ ps)
  where findPri Nothing = Nothing
        findPri (Just (pn :-> p, rest)) =
            if selector pn
                then pickAtPri p [pn] (minView rest)
                else findPri $ minView rest
        pickAtPri p acc Nothing = Just acc
        pickAtPri p acc (Just (pn :-> p', rest))
            | p == p' = if selector pn
                            then pickAtPri p (pn : acc) $ minView rest
                            else pickAtPri p acc $ minView rest
            | otherwise = Just acc

