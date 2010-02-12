-- | Simple abstraction for message digests
module Digest
  ( Digest
  , digest
  )
where

import Control.Concurrent
import Control.Monad

import Data.Maybe

import qualified Data.ByteString.Lazy as L
import OpenSSL
import qualified OpenSSL.EVP.Digest as D

-- Consider newtyping this
type Digest = String

sha1Digest :: IO D.Digest
sha1Digest = do dig <- D.getDigestByName "SHA1"
		case dig of
		    Nothing -> fail "No such digest, SHA1"
		    Just d -> return d

digest :: L.ByteString -> IO Digest
digest bs =
  runInBoundThread $ -- I don't think this is needed strictly
    withOpenSSL $
	do sha1 <- sha1Digest
	   return $ D.digestLBS sha1 bs

