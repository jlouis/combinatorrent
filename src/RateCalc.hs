-- | Rate calculation.
module RateCalc (
    -- * Types
      Rate
    -- * Interface
    , new
    , update
    )

where

import Prelude hiding (last)

type Time = Integer

-- | A Rate is a record of information used for calculating the rate
data Rate = Rate
    { rate :: Double -- ^ The current rate
    , bytes :: Integer -- ^ The amount of bytes transferred since last rate extraction
    , count :: Integer -- ^ The amount of bytes transferred since last count extraction
    , nextExpected :: Time -- ^ When is the next rate update expected
    , last :: Time          -- ^ When was the last rate update
    , rateSince :: Time     -- ^ From where is the rate measured
    }

fudge :: Integer
fudge = 5 -- Seconds

rateUpdate :: Integer
rateUpdate = 5 * 1000 -- Millisecs

maxRatePeriod :: Integer
maxRatePeriod = 20 -- Seconds

new :: Time -> Rate
new t = Rate { rate = 0.0
             , bytes = 0
	     , count = 0
	     , nextExpected = t + fudge
	     , last         = t - fudge
	     , rateSince    = t - fudge
	     }

-- | The call @update n rt@ updates the rate structure @rt@ with @n@ new bytes
update :: Integer -> Rate -> Rate
update n rt = rt { bytes = bytes rt + n
		 , count = count rt + n}

-- | The call @extractRate t rt@ extracts the current rate from the rate structure and updates the rate
--   structures internal book-keeping
extractRate :: Time -> Rate -> (Double, Rate)
extractRate t rt =
  let oldWindow = fromIntegral $ last rt - rateSince rt
      newWindow = fromIntegral $ t - rateSince rt
      n         = bytes rt
      r = (rate rt * oldWindow + (fromIntegral n)) / newWindow
  in
     -- Update the rate and book-keep the missing pieces. The total is simply a built-in
     -- counter. The point where we expect the next update is pushed at most 5 seconds ahead
     -- in time. But it might come earlier if the rate is high.
     -- Last is updated with the current time. Finally, we move the windows earliest value
     -- forward if it is more than 20 seconds from now.
	(r, rt { rate = r
	       , bytes = 0
	       , nextExpected = t + min 5 (round $ (fromIntegral n / (max r 0.0001)))
	       , last = t
	       , rateSince = max (rateSince rt) (t - maxRatePeriod)
	       })

-- | The call @extractCount rt@ extract the bytes transferred since last extraction
extractCount :: Rate -> (Integer, Rate)
extractCount rt = (count rt, rt { count = 0 })

