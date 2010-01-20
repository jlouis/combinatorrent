-- | Simple abstraction for message digests
module Digest
  ( Digest
  , digest
  )
where

import Control.Monad

import Data.Maybe

import qualified Data.ByteString.Lazy as L
import OpenSSL
import qualified OpenSSL.EVP.Digest as D

-- Consider newtyping this
type Digest = String

sha1Digest :: IO D.Digest
sha1Digest = liftM fromJust $ D.getDigestByName "SHA1"

digest :: L.ByteString -> IO Digest
digest bs =
    withOpenSSL $
	do sha1 <- sha1Digest
	   return $ D.digestLBS sha1 bs

