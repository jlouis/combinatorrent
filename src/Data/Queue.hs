-- | Simple Functional queues based on a double list. This usually achieves good amortized bounds
module Data.Queue (
              -- * Types
                Queue
              -- * Functions
              , empty
              , isEmpty
              , push
              , pop
              , Data.Queue.filter
              -- * Test Suite
              , testSuite
              )
where

import Data.List as Lst
import Data.Maybe (fromJust)

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Path, Test)

data Queue a = Queue [a] [a]
  deriving (Eq, Show)

-- | The definition of an empty Queue
empty :: Queue a
empty = Queue [] []

-- | Returns True on an empty Queue, and False otherwise.
isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

-- | Pushes a new element to the tail of the list.
--   Operates in constant time.
push :: a -> Queue a -> Queue a
push e (Queue front back) = Queue front (e : back)


-- | Pops the top most element off the queue.
--   Operates in amortized constant time
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue []       [])   = Nothing
pop (Queue (e : es) back) = Just (e, Queue es back)
pop (Queue []       back) = pop (Queue (reverse back) [])

-- | Generates a new Queue only containing elements for which
--   p returns true.
filter :: (a -> Bool) -> Queue a -> Queue a
filter p (Queue front back) = Queue (Lst.filter p front) (Lst.filter p back)


-- Tests

testSuite :: Test
testSuite = testGroup "Data/Queue"
  [ testCase "Empty Queue is Empty" testEmptyIsEmpty
  , testProperty "Simple push/pop" testPushPopSimple
  , testProperty "push/pop more"   testPushPopMore
  , testProperty "push/pop interleave" testPushPopInterleave
  ]

-- Rudimentary boring simple tests
testEmptyIsEmpty :: Assertion
testEmptyIsEmpty = do
    assertEqual "for Empty Q" (isEmpty empty) True
    assertEqual "for non-Empty Q" (isEmpty (push "Foo" empty)) False
    assertEqual "for popping the Empty Q" (pop $ snd . fromJust . pop $ push "Foo" empty) Nothing

testPushPopSimple :: String -> Bool
testPushPopSimple s =
    let nq = pop (push s empty)
    in case nq of
        Nothing -> False
        Just (r, q) -> r == s && isEmpty q

testPushPopMore :: [String] -> Bool
testPushPopMore ls =
    let nq = foldl (flip push) empty ls
        popAll = unfoldr pop nq
    in popAll == ls

data Operation = Push | Pop
  deriving (Eq, Show)

instance Arbitrary Operation where
    arbitrary = oneof [return Push, return Pop]

testPushPopInterleave :: [Operation] -> [String] -> Bool
testPushPopInterleave ops ls = testQ empty ops ls []
  where
    testQ q op lst res =
     case op of
        [] -> popAll q == reverse res
        Pop : r -> case pop q of
                     Nothing -> testQ empty r lst []
                     Just (elem, nq) ->
                        if (last res) == elem
                            then testQ nq r lst (init res)
                            else False
        Push : r -> case lst of
                     [] -> testQ q r lst res
                     (e : es) -> testQ (push e q) r es (e : res)
    popAll = unfoldr pop
