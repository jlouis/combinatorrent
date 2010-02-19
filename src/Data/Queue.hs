-- | Simple Functional queues based on a double list. This usually achieves good amortized bounds
module Data.Queue (
              -- * Types
              Queue,
              -- * Functions
              empty,
              isEmpty,
              push,
              pop,
              Data.Queue.filter)
where

import qualified Data.List as Lst

data Queue a = Queue [a] [a]

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
