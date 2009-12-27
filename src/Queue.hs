-- | Simple Functional queues based on a double list. This usually achieves good amortized bounds
--   TODO: Document me. I should be easy to document.
module Queue (Queue,
              empty,
              isEmpty,
              push,
              pop,
              Queue.filter)
where

import qualified Data.List as Lst

data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

push :: a -> Queue a -> Queue a
push e (Queue front back) = Queue front (e : back)


pop :: Queue a -> Maybe (a, Queue a)
pop (Queue []       [])   = Nothing
pop (Queue (e : es) back) = Just (e, Queue es back)
pop (Queue []       back) = pop (Queue (reverse back) [])

filter :: (a -> Bool) -> Queue a -> Queue a
filter p (Queue front back) = Queue (Lst.filter p front) (Lst.filter p back)