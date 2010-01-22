-- | Simple abstraction for message digests
module Digest
  ( Digest
  , digest
  )
where

import qualified Data.ByteString.Lazy as L
import qualified Data.Digest.Pure.SHA as P

-- Consider newtyping this
type Digest = L.ByteString

digest :: L.ByteString -> Digest
digest = P.bytestringDigest . P.sha1

