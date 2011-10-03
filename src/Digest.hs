-- | Simple abstraction for message digests
{-# LANGUAGE TypeSynonymInstances #-}
module Digest
  ( Digest
  , digest
  , digestBS
  )
where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.State

import Data.Word

import Foreign.Ptr
import qualified Data.ByteString as B
import Data.ByteString.Unsafe
import qualified Data.ByteString.Lazy as L
import qualified Crypto.Hash.SHA1 as SHA1

-- Consider newtyping this
type Digest = B.ByteString

instance NFData Digest

digest :: L.ByteString -> B.ByteString
digest bs = {-# SCC "sha1_digest" #-} SHA1.hashlazy bs

digestBS :: B.ByteString -> B.ByteString
digestBS bs = digest . L.fromChunks $ [bs]
