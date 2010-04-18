-- | Simple abstraction for message digests
{-# LANGUAGE TypeSynonymInstances #-}
module Digest
  ( Digest
  , digest
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
import qualified OpenSSL.Digest as SSL

-- Consider newtyping this
type Digest = B.ByteString

instance NFData Digest

digest :: L.ByteString -> IO B.ByteString
digest bs = {-# SCC "sha1_digest" #-} B.pack <$> digestLBS SSL.SHA1 bs

digestLBS :: SSL.MessageDigest -> L.ByteString -> IO [Word8]
digestLBS mdType xs = {-# SCC "sha1_digestLBS" #-}
  SSL.mkDigest mdType $ evalStateT (updateLBS xs >> SSL.final)

updateBS :: B.ByteString -> SSL.Digest ()
updateBS bs = {-# SCC "sha1_updateBS" #-} do
    SSL.DST ctx <- get
    _ <- liftIO $ unsafeUseAsCStringLen bs $
            \(ptr, len) -> SSL.digestUpdate ctx (castPtr ptr) (fromIntegral len)
    return ()

updateLBS :: L.ByteString -> SSL.Digest ()
updateLBS lbs = {-# SCC "sha1_updateLBS" #-} mapM_ updateBS chunked
  where chunked = {-# SCC "sha1_updateLBS_chunked" #-} L.toChunks lbs
