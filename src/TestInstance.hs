-- Define a set of test instances of common types
-- Portions of this code is taken from "Real World Haskell"
module TestInstance
    ()
where


import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L


import Test.QuickCheck


{-# LANGUAGE CPP #-}
#if MIN_VERSION_random(1,0,1)
-- random>=1.0.1 is exporting these instances, so don't need to redefine it
#else
 
import Data.Word
import System.Random

integralRandomR :: (Integral a, Integral b, RandomGen g, Num b) => (a, b) -> g -> (b, g)
integralRandomR (a,b) g = case randomR (c,d) g of
                            (x,h) -> (fromIntegral x, h)
    where (c,d) = (fromIntegral a :: Integer,
                   fromIntegral b :: Integer)

instance Random Word32 where
  randomR = integralRandomR
  random = randomR (minBound, maxBound)

instance Random Word8 where
    randomR = integralRandomR
    random = randomR (minBound, maxBound)
#endif

instance Arbitrary L.ByteString where
    arbitrary = L.pack `fmap` arbitrary

instance CoArbitrary L.ByteString where
    coarbitrary = coarbitrary . L.unpack

instance Arbitrary B.ByteString where
    arbitrary = B.pack `fmap` arbitrary

instance CoArbitrary B.ByteString where
    coarbitrary = coarbitrary . B.unpack

