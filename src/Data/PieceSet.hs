-- | Module for the representation of PieceSets. Exist so we can abstract on the implementation later
module Data.PieceSet
    ( PieceSet
    , new
    , size
    , full
    , delete
    , union
    , null
    , insert
    , intersection
    , member
    , fromList
    , toList
    , elems
    -- * Tests
    , testSuite
    )
where

import Control.DeepSeq
import qualified Data.IntSet as IS
import Prelude hiding (null)

--import Test.QuickCheck
import Test.Framework
--import Test.Framework.Providers.QuickCheck2
--import Test.Framework.Providers.HUnit
--import Test.HUnit hiding (Path, Test)

data PieceSet = PSet { unPSet :: IS.IntSet
                     , unSz   :: Int }
  deriving Show

instance NFData PieceSet where
    rnf (PSet is _) = rnf is

new :: Int -> PieceSet
new n = PSet IS.empty n

null :: PieceSet -> Bool
null = IS.null . unPSet

insert :: Int -> PieceSet -> PieceSet
insert n (PSet ps i) = PSet (IS.insert n ps) i

full :: PieceSet -> Bool
full ps = all (flip IS.member (unPSet ps)) [1..unSz ps]

size :: PieceSet -> Int
size = IS.size . unPSet

member :: Int -> PieceSet -> Bool
member n = IS.member n . unPSet

delete :: Int -> PieceSet -> PieceSet
delete n (PSet ps i) = PSet (IS.delete n ps) i

intersection :: PieceSet -> PieceSet -> PieceSet
intersection (PSet ps1 i1) (PSet ps2 i2) | i1 /= i2 = error "Wrong PSet intersection"
                                         | otherwise = PSet (IS.intersection ps1 ps2) i1

union :: PieceSet -> PieceSet -> PieceSet
union (PSet ps1 i1) (PSet ps2 i2) | i1 /= i2 = error "Wrong PSet union"
                                  | otherwise = PSet (IS.union ps1 ps2) i1

fromList :: Int -> [Int] -> PieceSet
fromList n elems = PSet (IS.fromList elems) n

toList :: PieceSet -> [Int]
toList = IS.toList . unPSet

elems :: PieceSet -> [Int]
elems = toList

-- Tests

testSuite :: Test
testSuite = testGroup "Data/PieceSet"
    []

