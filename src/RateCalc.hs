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
    , total :: Integer -- ^ The total amount of bytes downloaded
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
             , total = 0
	     , nextExpected = t + fudge
	     , last         = t - fudge
	     , rateSince    = t - fudge
	     }

-- | The call @update t n rt@ updates the rate @rt@ at time @t@ with @n@ bytes of data. Returns the
--   new rate
update :: Time -> Integer -> Rate -> Rate
update t n rt =
    -- This case is true when we are requested to update with 0 bytes before the rate is exhausted
    --   we can probably kill this in the Haskell client.
    if t < nextExpected rt && n == 0
	then rt
	-- Calculation of the new rate. The timeslot between last and rateSince gives a window in
	-- which the current rate was observed. It contributes with some bytes. The @n@ amount
	-- contributes the new bytes we have added. To get the new rate, we divide to the new extended
	-- time slot window.
	else let oldWindow = fromIntegral $ last rt - rateSince rt
		 newWindow = fromIntegral $ t - rateSince rt
		 r = (rate rt * oldWindow + (fromIntegral n)) / newWindow
	     in
	     -- Update the rate and book-keep the missing pieces. The total is simply a built-in
	     -- counter. The point where we expect the next update is pushed at most 5 seconds ahead
	     -- in time. But it might come earlier if the rate is high.
	     -- Last is updated with the current time. Finally, we move the windows earliest value
	     -- forward if it is more than 20 seconds from now.
		rt { rate = r
		   , total = total rt + n
		   , nextExpected = t + min 5 (round $ (fromIntegral n / (max r 0.0001)))
		   , last = t
		   , rateSince = max (rateSince rt) (t - maxRatePeriod)
		}

