-- | Simple Functional queues based on a double list. This usually achieves good amortized bounds
module Queue (Queue,
              empty,
              isEmpty,
              push,
              pop )
where

data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

push :: Queue a -> a -> Queue a
push (Queue front back) = Queue front . (: back)


pop :: Queue a -> Maybe (a, Queue a)
pop (Queue []       [])   = Nothing
pop (Queue (e : es) back) = Just (e, Queue es back)
pop (Queue [] back)       = pop (Queue (reverse back) [])

