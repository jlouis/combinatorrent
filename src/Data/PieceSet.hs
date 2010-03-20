-- | Module for the representation of PieceSets. Exist so we can abstract on the implementation later
module Data.PieceSet
    ( PieceSet
    , new
    , size
    , delete
    , union
    , null
    , insert
    , intersection
    , member
    , fromList
    , toList
    , elems
    )
where

import Control.DeepSeq
import qualified Data.IntSet as IS
import Prelude hiding (null)

newtype PieceSet = PSet { unPSet :: IS.IntSet }
    deriving Show

instance NFData PieceSet where
    rnf (PSet is) = rnf is

new :: Int -> PieceSet
new _ = PSet IS.empty

null :: PieceSet -> Bool
null = IS.null . unPSet

insert :: Int -> PieceSet -> PieceSet
insert n = PSet . IS.insert n . unPSet

size :: PieceSet -> Int
size = IS.size . unPSet

member :: Int -> PieceSet -> Bool
member n = IS.member n . unPSet

delete :: Int -> PieceSet -> PieceSet
delete n = PSet . IS.delete n . unPSet

intersection :: PieceSet -> PieceSet -> PieceSet
intersection p1 p2 = PSet $ IS.intersection (unPSet p1) (unPSet p2)

union :: PieceSet -> PieceSet -> PieceSet
union p1 p2 = PSet $ IS.union (unPSet p1) (unPSet p2)

fromList :: [Int] -> PieceSet
fromList = PSet . IS.fromList

toList :: PieceSet -> [Int]
toList = IS.toList . unPSet

elems :: PieceSet -> [Int]
elems = toList
