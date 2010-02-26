-- | Simple abstraction for message digests
module Digest
  ( Digest
  , digest
  )
where

import Data.Char

import qualified Data.ByteString.Lazy as L
import qualified OpenSSL.Digest as SSL hiding (Digest)

-- Consider newtyping this
type Digest = String

digest :: L.ByteString -> IO Digest
digest bs = do
    upack <- SSL.digest SSL.SHA1 $ L.unpack bs
    return $ map (chr . fromIntegral) upack

