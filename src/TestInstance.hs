-- Define a set of test instances of common types
-- Portions of this code is taken from "Real World Haskell"
module TestInstance
    ()
where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import System.Random
import Test.QuickCheck

integralRandomR :: (Integral a, Integral b, RandomGen g, Num b) => (a, b) -> g -> (b, g)
integralRandomR (a,b) g = case randomR (c,d) g of
                            (x,h) -> (fromIntegral x, h)
    where (c,d) = (fromIntegral a :: Integer,
                   fromIntegral b :: Integer)

instance Random Word32 where
  randomR = integralRandomR
  random = randomR (minBound, maxBound)

instance Arbitrary Word32 where
    arbitrary = arbitraryBoundedRandom

instance CoArbitrary Word32 where
    coarbitrary = coarbitraryIntegral

instance Random Word8 where
    randomR = integralRandomR
    random = randomR (minBound, maxBound)

instance Arbitrary Word8 where
    arbitrary = arbitraryBoundedRandom

instance CoArbitrary Word8 where
    coarbitrary = coarbitraryIntegral

instance Arbitrary L.ByteString where
    arbitrary = L.pack `fmap` arbitrary

instance CoArbitrary L.ByteString where
    coarbitrary = coarbitrary . L.unpack

instance Arbitrary B.ByteString where
    arbitrary = B.pack `fmap` arbitrary

instance CoArbitrary B.ByteString where
    coarbitrary = coarbitrary . B.unpack

