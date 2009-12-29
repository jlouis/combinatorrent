
-- | Overwritable mailboxes.
--   An overwritable mailbox is like a SampleMVar in Haskells basic low-level concurrency, but
--   implemented as a CML primitive.
module OMBox
where

import Control.Concurrent.CML

new :: IO (Channel a, Channel a)
new = do putterC <- channel
         getterC <- channel
         spawn $ lp putterC getterC Nothing
         return (putterC, getterC)
  where lp putCh getCh Nothing = sync (putEvent putCh) >>= lp putCh getCh
        lp putCh getCh (Just v) =
            sync (choose [putEvent putCh, getEvent getCh v]) >>= lp putCh getCh
        putEvent c = wrap (receive c (const True))
                      (return . Just)
        getEvent c v = wrap (transmit c v) (const $ return Nothing)