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
    , Data.PieceSet.freeze
    -- * Tests
    , testSuite
    )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Array.IO
import Data.Array.Unboxed ((!), UArray)
import qualified Data.Foldable as F
import Data.List ((\\), partition, sort, null)
import Prelude hiding (null)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Path, Test)
import TestInstance() -- Pull arbitraries

import Torrent

newtype PieceSet = PieceSet { unPieceSet :: IOUArray Int Bool }

new :: MonadIO m => Int -> m PieceSet
new n = {-# SCC "Data.PieceSet/new" #-}
    liftIO $ PieceSet <$> newArray (0, n-1) False

all :: MonadIO m => (Bool -> Bool) -> PieceSet -> m Bool
all f ps = liftIO $ do
    elems <- getElems $ unPieceSet ps
    return $ Prelude.all f elems

null :: MonadIO m => PieceSet -> m Bool
null = Data.PieceSet.all (==False) 

full :: MonadIO m => PieceSet -> m Bool
full = {-# SCC "Data.PieceSet/full" #-} Data.PieceSet.all (==True)

insert :: MonadIO m => Int -> PieceSet -> m ()
insert n (PieceSet ps) = {-# SCC "Data.PieceSet/insert" #-}
    liftIO $ writeArray ps n True

size :: MonadIO m => PieceSet -> m Int
size (PieceSet arr) = {-# SCC "Data.PieceSet/size" #-}
    liftIO $ do
        (l, u) <- getBounds arr
        walk [l..u] 0
 where walk [] n       = return n
       walk (x : xs) n = readArray arr x >>= \p -> if p then walk xs (n+1) else walk xs n

member :: MonadIO m => Int -> PieceSet -> m Bool
member n (PieceSet arr) = {-# SCC "Data.PieceSet/member" #-}
    liftIO $ readArray arr n

delete :: MonadIO m => Int -> PieceSet -> m ()
delete n (PieceSet arr) = {-# SCC "Data.PieceSet/delete" #-} liftIO $
    writeArray arr n False

intersection :: MonadIO m => PieceSet -> PieceSet -> m [Int]
intersection (PieceSet arr1) (PieceSet arr2) = liftIO $ do
    eqSize <- (==) <$> getBounds arr1 <*> getBounds arr2
    if not eqSize
        then error "Wrong intersection sizes"
        else do
            elems <- getAssocs arr1
            F.foldlM mem [] elems
  where
    mem ls (_, False) = return ls
    mem ls (i, True)  = do
            m <- readArray arr2 i
            return $ if m then (i : ls) else ls


fromList :: MonadIO m => Int -> [Int] -> m PieceSet
fromList n elems = {-# SCC "Data.PieceSet/fromList" #-} liftIO $ do
    nArr <- newArray (0, n-1) False
    mapM_ (flip (writeArray nArr) True) elems
    return $ PieceSet nArr

toList :: MonadIO m => PieceSet -> m [Int]
toList (PieceSet arr) = {-# SCC "Data.PieceSet/toList" #-} liftIO $ do
    elems <- getAssocs arr
    return [i | (i, e) <- elems, e == True]

freeze :: MonadIO m => PieceSet -> m (PieceNum -> Bool)
freeze (PieceSet ps) = do
    frozen <- liftIO $ (Data.Array.IO.freeze ps :: IO (UArray Int Bool))
    return $ (frozen !)

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
    ]

testNewSize :: Assertion
testNewSize = do
    a <- new 1337
    sz <- size a
    assertEqual "For a new PieceSet" sz 0
    insert 3 a
    insert 5 a
    sz2 <- size a
    assertEqual "For inserted" sz2 2

testFull :: Assertion
testFull = do
    let maxElem = 1337
    ps <- new maxElem
    _ <- forM [0..maxElem-1] (flip insert ps)
    tst <- liftM and $ mapM (flip member ps) [0..maxElem-1]
    assertBool "for a full PieceSet" tst

testBuild :: Assertion
testBuild = do
    let nums = [0..1336]
        m = 1336 + 1
    ps <- fromList m nums
    sz <- size ps
    assertEqual "for size" sz (length nums)

testIntersect :: Assertion
testIntersect = do
    let (evens, odds) = partition (\x -> x `mod` 2 == 0) [0..99]
    evPS <- fromList 100 evens
    oddPS <- fromList 100 odds
    is1 <- intersection evPS oddPS
    assertBool "for intersection" (Data.List.null is1)
    ps1 <- fromList 10 [1,2,3,4,9]
    ps2 <- fromList 10 [0,2,5,4,8 ]
    is2 <- intersection ps1 ps2
    assertBool "for simple intersection" (sort is2 == [2,4])

testMember :: Assertion
testMember = do
    let evens = filter (\x -> x `mod` 2 == 0) [0..999]
        m     = 1000
        notThere = [0..999] \\ evens
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

