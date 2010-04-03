module Tracer
    ( Tracer
    , new
    , trace
    )
where

data Tracer a = Tracer [a] [a] Int Int

instance Show a => Show (Tracer a) where
    show (Tracer cur old _ _) = show (cur ++ old)

new :: Int -> Tracer a
new x = Tracer [] [] 0 x

trace :: a -> Tracer a -> Tracer a
trace msg (Tracer cur old sz l)
    | sz == l   = Tracer [msg] cur 0 l
    | otherwise = Tracer (msg : cur) old (sz+1) l

