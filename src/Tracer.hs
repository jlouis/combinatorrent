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
trace msg (Tracer cur old sz length)
    | sz == length = Tracer [msg] cur 0 length
    | otherwise    = Tracer (msg : cur) old (sz+1) length

