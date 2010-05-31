-- | Rate calculation.
{-# LANGUAGE BangPatterns #-}
module RateCalc (
    -- * Types
      Rate
    -- * Interface
    , new
    , update
    , extractCount
    , extractRate
    )

where

import Control.DeepSeq
import Data.Time.Clock

-- | A Rate is a record of information used for calculating the rate
data Rate = Rate
    { rate  :: !Double -- ^ The current rate
    , bytes :: !Int -- ^ The amount of bytes transferred since last rate extraction
    , count :: !Int -- ^ The amount of bytes transferred since last count extraction
    , lastExt      :: !UTCTime -- ^ When was the last rate update
    , rateSince    :: !UTCTime -- ^ From where is the rate measured
    }

instance NFData Rate where
    rnf (Rate r b c _ _) =
        rnf r `seq` rnf b `seq` rnf c

fudge :: NominalDiffTime
fudge = fromInteger 5 -- Seconds

maxRatePeriod :: NominalDiffTime
maxRatePeriod = fromInteger 20 -- Seconds

new :: UTCTime -> Rate
new t = Rate { rate = 0.0
             , bytes = 0
             , count = 0
             , lastExt      = addUTCTime (-fudge) t
             , rateSince    = addUTCTime (-fudge) t
             }

-- | The call @update n rt@ updates the rate structure @rt@ with @n@ new bytes
update :: Int -> Rate -> Rate
update n rt = {-# SCC "update" #-}
        rt { bytes = nb, count = nc}
  where !nb = bytes rt + n
        !nc = count rt + n


-- | The call @extractRate t rt@ extracts the current rate from the rate
-- structure and updates the rate structures internal book-keeping
extractRate :: UTCTime -> Rate -> (Double, Rate)
extractRate t rt = {-# SCC "extractRate" #-}
  let since = rateSince rt
      lext  = lastExt rt
      n     = bytes rt
      oldWindow :: Double
      oldWindow = {-# SCC "diffUTC1" #-} realToFrac $ diffUTCTime lext since
      newWindow :: Double
      newWindow = {-# SCC "diffUTS2" #-} realToFrac $ diffUTCTime t since
      !r = {-# SCC "r" #-} (rate rt * oldWindow + (fromIntegral n)) / newWindow
      !nrt = {-# SCC "rt_creat" #-}
             rt { rate = r
                , bytes = 0
                , lastExt = t
                , rateSince = {-# SCC "max" #-} max since (addUTCTime (-maxRatePeriod) t)
                }
  in
     -- Update the rate and book-keep the missing pieces. The total is simply a built-in
     -- counter. The point where we expect the next update is pushed at most 5 seconds ahead
     -- in time. But it might come earlier if the rate is high.
     -- Last is updated with the current time. Finally, we move the windows earliest value
     -- forward if it is more than 20 seconds from now.
     (r, nrt)

-- | The call @extractCount rt@ extract the bytes transferred since last extraction
extractCount :: Rate -> (Int, Rate)
extractCount rt = {-# SCC "extractCount" #-} (crt, rt { count = 0 })
  where crt = count rt

