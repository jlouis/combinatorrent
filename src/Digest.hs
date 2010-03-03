-- | Simple abstraction for message digests
module Digest
  ( Digest
  , digest
  )
where

import Data.Char
import Data.Word
import Control.Monad.State

import Foreign.Ptr
import qualified Data.ByteString as B
import Data.ByteString.Unsafe
import qualified Data.ByteString.Lazy as L
import qualified OpenSSL.Digest as SSL

-- Consider newtyping this
type Digest = String

digest :: L.ByteString -> IO Digest
digest bs = do
    upack <- digestLBS SSL.SHA1 bs
    return $ map (chr . fromIntegral) upack

digestLBS :: SSL.MessageDigest -> L.ByteString -> IO [Word8]
digestLBS mdType xs =
  SSL.mkDigest mdType $ evalStateT (updateLBS xs >> SSL.final)

updateBS :: B.ByteString -> SSL.Digest ()
updateBS bs = do
    SSL.DST ctx <- get
    liftIO $ unsafeUseAsCStringLen bs $
            \(ptr, len) -> SSL.digestUpdate ctx (castPtr ptr) (fromIntegral len)
    return ()

updateLBS :: L.ByteString -> SSL.Digest ()
updateLBS lbs = mapM_ updateBS $ L.toChunks lbs
