-- | Module for the representation of PieceSets. Exist so we can abstract on the implementation later
module Data.PieceSet
    ( PieceSet
    , new
    , size
    , full
    , copy
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

import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import qualified Data.IntSet as IS
import Data.IORef
import Data.List ((\\), partition, sort, null)
import Prelude hiding (null)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Path, Test)
import TestInstance() -- Pull arbitraries

data PieceSet = PSet { unPSet :: IORef IS.IntSet
                     , unSz   :: !Int }

instance NFData PieceSet where
    rnf (PSet is _) = ()

new :: MonadIO m => Int -> m PieceSet
new n = {-# SCC "Data.PieceSet/new" #-} do
    s <- liftIO $ newIORef IS.empty
    liftIO . return $ PSet s n

null :: MonadIO m => PieceSet -> m Bool
null ps = do
    s <- (liftIO . readIORef . unPSet) ps
    liftIO $ return $ IS.null s

insert :: MonadIO m => Int -> PieceSet -> m ()
insert n (PSet ps i) = {-# SCC "Data.PieceSet/insert" #-} do
    liftIO $ atomicModifyIORef ps (\ps -> (IS.insert n ps, ()))

full :: MonadIO m => PieceSet -> m Bool
full ps = {-# SCC "Data.PieceSet/full" #-} do
    s <- liftIO . readIORef . unPSet $ ps
    liftIO . return $ all (flip IS.member s) [1..unSz ps]

copy :: MonadIO m => PieceSet -> m PieceSet
copy (PSet ps sz) = do
    s <- liftIO . readIORef $ ps
    o <- liftIO $ newIORef s
    liftIO $ return $ PSet o sz

size :: MonadIO m => PieceSet -> m Int
size = {-# SCC "Data.PieceSet/size" #-}
    liftIO . liftM IS.size . readIORef . unPSet

member :: MonadIO m => Int -> PieceSet -> m Bool
member n = {-# SCC "Data.PieceSet/member" #-}
    liftIO . liftM (IS.member n) . readIORef . unPSet

delete :: MonadIO m => Int -> PieceSet -> m ()
delete n (PSet ps i) = {-# SCC "Data.PieceSet/delete" #-}
    liftIO $ atomicModifyIORef ps (\ps -> (IS.delete n ps, ()))

intersection :: MonadIO m => PieceSet -> PieceSet -> m [Int]
intersection (PSet ps1 i1) (PSet ps2 i2)
                | i1 /= i2 = error "Wrong PSet intersection"
                | otherwise = {-# SCC "Data.PieceSet/intersection" #-} do
                            ps11 <- liftIO $ readIORef ps1
                            ps21 <- liftIO $ readIORef ps2
                            return $ IS.toList $ IS.intersection ps11 ps21

fromList :: MonadIO m => Int -> [Int] -> m PieceSet
fromList n elems = {-# SCC "Data.PieceSet/fromList" #-} do
    s <- liftIO $ newIORef (IS.fromList elems)
    liftIO . return $ PSet s n

toList :: MonadIO m => PieceSet -> m [Int]
toList = {-# SCC "Data.PieceSet/toList" #-}
    liftIO . liftM IS.toList . readIORef . unPSet

-- Tests

testSuite :: Test
testSuite = testGroup "Data/PieceSet"
    [ testCase "New/Size" testNewSize
    , testCase "Full"  testFull
    , testCase "Build" testBuild
    , testCase "Full" testFull
    , testCase "Intersection" testIntersect
    , testCase "Membership" testMember
    , testCase "Insert/Delete" testInsertDelete
    , testCase "Copy" testCopy
    ]

testNewSize :: Assertion
testNewSize = do
    sz <- size =<< new 1337
    assertEqual "For a new PieceSet" sz 0

testFull :: Assertion
testFull = do
    let maxElem = 1337
    ps <- new maxElem
    forM [0..maxElem-1] (flip insert ps)
    tst <- liftM and $ mapM (flip member ps) [0..maxElem-1]
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

testInsertDelete :: Assertion
testInsertDelete = do
    ps <- new 10
    insert 3 ps
    insert 4 ps
    assertBool "Ins/del #1" =<< member 3 ps
    assertBool "Ins/del #2" =<< liftM not (member 5 ps)
    delete 3 ps
    assertBool "Ins/del #3" =<< member 4 ps
    assertBool "Ins/del #4" =<< liftM not (member 3 ps)
    insert 5 ps
    assertBool "Ins/del #5" =<< member 5 ps

testCopy :: Assertion
testCopy = do
    ps <- new 10
    insert 3 ps
    pc <- copy ps
    insert 4 pc
    delete 3 pc
    assertBool "#1" =<< member 3 ps
    assertBool "#2" =<< liftM not (member 3 pc)
    assertBool "#3" =<< member 4 pc
    assertBool "#4" =<< liftM not (member 4 ps)


