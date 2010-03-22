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
    -- * Tests
    , testSuite
    )
where

import Control.DeepSeq
import Control.Monad.Trans
import qualified Data.IntSet as IS
import Data.List (nub)
import Data.Word
import Prelude hiding (null)

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Path, Test)
import TestInstance() -- Pull arbitraries

data PieceSet = PSet { unPSet :: !IS.IntSet
                     , unSz   :: !Int }
  deriving Show

instance NFData PieceSet where
    rnf (PSet is _) = rnf is

new :: Int -> PieceSet
new n = {-# SCC "Data.PieceSet/new" #-} PSet IS.empty n

null :: MonadIO m => PieceSet -> m Bool
null = liftIO . return . IS.null . unPSet

insert :: Int -> PieceSet -> PieceSet
insert n (PSet ps i) = {-# SCC "Data.PieceSet/insert" #-} PSet (IS.insert n ps) i

full :: PieceSet -> Bool
full ps = {-# SCC "Data.PieceSet/full" #-} all (flip IS.member (unPSet ps)) [1..unSz ps]

size :: PieceSet -> Int
size = {-# SCC "Data.PieceSet/size" #-} IS.size . unPSet

member :: Int -> PieceSet -> Bool
member n = {-# SCC "Data.PieceSet/member" #-} IS.member n . unPSet

delete :: Int -> PieceSet -> PieceSet
delete n (PSet ps i) = {-# SCC "Data.PieceSet/delete" #-} PSet (IS.delete n ps) i

intersection :: PieceSet -> PieceSet -> PieceSet
intersection (PSet ps1 i1) (PSet ps2 i2) | i1 /= i2 = error "Wrong PSet intersection"
                                         | otherwise = {-# SCC "Data.PieceSet/intersection" #-}
                                                            PSet (IS.intersection ps1 ps2) i1

union :: PieceSet -> PieceSet -> PieceSet
union (PSet ps1 i1) (PSet ps2 i2) | i1 /= i2 = error "Wrong PSet union"
                                  | otherwise = {-# SCC "Data.PieceSet/union" #-}
                                                    PSet (IS.union ps1 ps2) i1

fromList :: Int -> [Int] -> PieceSet
fromList n elems = {-# SCC "Data.PieceSet/fromList" #-} PSet (IS.fromList elems) n

toList :: PieceSet -> [Int]
toList = {-# SCC "Data.PieceSet/toList" #-} IS.toList . unPSet

-- Tests

testSuite :: Test
testSuite = testGroup "Data/PieceSet"
    [ testCase "New/Size" testNewSize
    , testProperty "Build" testBuild
    , testProperty "Full"  testFull
    ]

testNewSize :: Assertion
testNewSize = do
    assertEqual "For a new PieceSet" (size (new 1337)) 0

testFull :: Positive Word8 -> Bool
testFull positive =
    let maxElem = fromIntegral positive
        pieceSet = foldl (flip insert) (new maxElem) [0..maxElem-1]
    in all (flip member pieceSet) [0..maxElem-1]

testBuild :: [Positive Int] -> Bool
testBuild []        = True
testBuild positives =
    let m = fromIntegral $ maximum positives
        nubbed = nub positives
    in length nubbed == size (fromList m $ map fromIntegral nubbed)
