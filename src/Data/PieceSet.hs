-- | Module for the representation of PieceSets. Exist so we can abstract on the implementation later
module Data.PieceSet
    ( PieceSet
    , new
    , size
    , full
    , delete
    , Data.PieceSet.null
    , insert
    , intersection
    , member
    , fromList
    , toList
    -- * Tests
    , testSuite
    )
where

--import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import qualified Data.IntSet as IS
import Data.List ((\\), partition, sort, null)
import Prelude hiding (null)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Path, Test)
import TestInstance() -- Pull arbitraries

data PieceSet = PSet { unPSet :: !IS.IntSet
                     , unSz   :: !Int }
  deriving Show

instance NFData PieceSet where
    rnf (PSet is _) = rnf is

new :: MonadIO m => Int -> m PieceSet
new n = {-# SCC "Data.PieceSet/new" #-} liftIO $ return $ PSet IS.empty n

null :: MonadIO m => PieceSet -> m Bool
null = liftIO . return . IS.null . unPSet

insert :: Int -> PieceSet -> PieceSet
insert n (PSet ps i) = {-# SCC "Data.PieceSet/insert" #-} PSet (IS.insert n ps) i

full :: MonadIO m => PieceSet -> m Bool
full ps = {-# SCC "Data.PieceSet/full" #-}
    liftIO . return $ all (flip IS.member (unPSet ps)) [1..unSz ps]

size :: MonadIO m => PieceSet -> m Int
size = {-# SCC "Data.PieceSet/size" #-}
    liftIO . return . IS.size . unPSet

member :: MonadIO m => Int -> PieceSet -> m Bool
member n = {-# SCC "Data.PieceSet/member" #-} liftIO . return . IS.member n . unPSet

delete :: Int -> PieceSet -> PieceSet
delete n (PSet ps i) = {-# SCC "Data.PieceSet/delete" #-} PSet (IS.delete n ps) i

intersection :: MonadIO m => PieceSet -> PieceSet -> m [Int]
intersection (PSet ps1 i1) (PSet ps2 i2)
                | i1 /= i2 = error "Wrong PSet intersection"
                | otherwise = {-# SCC "Data.PieceSet/intersection" #-}
                           toList $ PSet (IS.intersection ps1 ps2) i1

fromList :: MonadIO m => Int -> [Int] -> m PieceSet
fromList n elems = {-# SCC "Data.PieceSet/fromList" #-} liftIO . return $ PSet (IS.fromList elems) n

toList :: MonadIO m => PieceSet -> m [Int]
toList = {-# SCC "Data.PieceSet/toList" #-}
    liftIO . return . IS.toList . unPSet

-- Tests

testSuite :: Test
testSuite = testGroup "Data/PieceSet"
    [ testCase "New/Size" testNewSize
    , testCase "Full"  testFull
    , testCase "Build" testBuild
    , testCase "Full" testFull
    , testCase "Intersection" testIntersect
    , testCase "Membership" testMember
    ]

testNewSize :: Assertion
testNewSize = do
    sz <- size =<< new 1337
    assertEqual "For a new PieceSet" sz 0

testFull :: Assertion
testFull = do
    let maxElem = 1337
    ps <- new maxElem
    let pieceSet = foldl (flip insert) ps [0..maxElem-1]
    tst <- liftM and $ mapM (flip member pieceSet) [0..maxElem-1]
    assertBool "for a full PieceSet" tst

testBuild :: Assertion
testBuild = do
    let positives = [1..1337]
        m = maximum positives
    ps <- fromList m positives
    sz <- size ps
    assertEqual "for size" sz (length positives)

testIntersect :: Assertion
testIntersect = do
    let (evens, odds) = partition (\x -> x `mod` 2 == 0) [1..100]
    evPS <- fromList 100 evens
    oddPS <- fromList 100 odds
    is1 <- intersection evPS oddPS
    assertBool "for intersection" (Data.List.null is1)
    ps1 <- fromList 10 [1,2,3,4,10]
    ps2 <- fromList 10 [0,2,5,4,8 ]
    is2 <- intersection ps1 ps2
    assertBool "for simple intersection" (sort is2 == [2,4])

testMember :: Assertion
testMember = do
    let evens = filter (\x -> x `mod` 2 == 0) [1..1000]
        m     = maximum evens
        notThere = [1..m] \\ evens
    ps <- fromList m evens
    a <- liftM and $ mapM (flip member ps) evens
    b <- liftM and $ mapM (liftM not . flip member ps) notThere
    assertBool "for members" a
    assertBool "for non-members" b

