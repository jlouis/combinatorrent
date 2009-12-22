-----------------------------------------------------------------------------
-- |
-- Module      : Data.ByteString.Builder
-- Copyright   : Lennart Kolmodin, Ross Paterson, George Giorgidze
-- License     : BSD3
-- 
-- Maintainer  : George Giorgidze <http://cs.nott.ac.uk/~ggg/>
-- Stability   : experimental
-- Portability : Portable
--
-- Efficient construction of lazy bytestrings.
--
-----------------------------------------------------------------------------
module Data.ByteString.Builder (

    -- * The Builder type
      Builder
    , toLazyByteString

    -- * Constructing Builders
    , empty
    , singleton
    , putWord8
    , putInt8
    , append
    , fromByteString        -- :: S.ByteString -> Builder
    , fromLazyByteString    -- :: L.ByteString -> Builder
    , putString

    -- * Flushing the buffer state
    , flush

    -- * Derived Builders
    -- ** Big-endian writes
    , putWord16be           -- :: Word16 -> Builder
    , putWord24be           -- :: Word32 -> Builder
    , putWord32be           -- :: Word32 -> Builder
    , putWord64be           -- :: Word64 -> Builder

    , putInt16be           -- :: Int16 -> Builder
    , putInt32be           -- :: Int32 -> Builder
    , putInt64be           -- :: Int64 -> Builder

    -- ** Little-endian writes
    , putWord16le           -- :: Word16 -> Builder
    , putWord24le           -- :: Word32 -> Builder
    , putWord32le           -- :: Word32 -> Builder
    , putWord64le           -- :: Word64 -> Builder

    , putInt16le           -- :: Int16 -> Builder
    , putInt32le           -- :: Int32 -> Builder
    , putInt64le           -- :: Int64 -> Builder

    -- ** Host-endian, unaligned writes
    , putWordHost           -- :: Word -> Builder
    , putWord16host         -- :: Word16 -> Builder
    , putWord32host         -- :: Word32 -> Builder
    , putWord64host         -- :: Word64 -> Builder
    -- Variable length numbers
    , putVarLenBe
    , putVarLenLe

  ) where

import Foreign
import Data.Monoid
import Data.Word
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

import Data.ByteString.Internal (inlinePerformIO,c2w)
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy.Internal as L

------------------------------------------------------------------------

-- | A 'Builder' is an efficient way to build lazy 'L.ByteString's.
-- There are several functions for constructing 'Builder's, but only one
-- to inspect them: to extract any data, you have to turn them into lazy
-- 'L.ByteString's using 'toLazyByteString'.
--
-- Internally, a 'Builder' constructs a lazy 'L.Bytestring' by filling byte
-- arrays piece by piece.  As each buffer is filled, it is \'popped\'
-- off, to become a new chunk of the resulting lazy 'L.ByteString'.
-- All this is hidden from the user of the 'Builder'.

newtype Builder = Builder {
        -- Invariant (from Data.ByteString.Lazy):
        --      The lists include no null ByteStrings.
        runBuilder :: (Buffer -> [S.ByteString]) -> Buffer -> [S.ByteString]
    }

instance Monoid Builder where
    mempty  = empty
    mappend = append

------------------------------------------------------------------------

-- | /O(1)./ The empty Builder, satisfying
--
--  * @'toLazyByteString' 'empty' = 'L.empty'@
--
empty :: Builder
empty = Builder id

-- | /O(1)./ A Builder taking a single byte, satisfying
--
--  * @'toLazyByteString' ('singleton' b) = 'L.singleton' b@
--
singleton :: Word8 -> Builder
singleton = writeN 1 . flip poke

putWord8 :: Word8 -> Builder
putWord8 = singleton
------------------------------------------------------------------------

-- | /O(1)./ The concatenation of two Builders, an associative operation
-- with identity 'empty', satisfying
--
--  * @'toLazyByteString' ('append' x y) = 'L.append' ('toLazyByteString' x) ('toLazyByteString' y)@
--
append :: Builder -> Builder -> Builder
append (Builder f) (Builder g) = Builder (f . g)

-- | /O(1)./ A Builder taking a 'S.ByteString', satisfying
--
--  * @'toLazyByteString' ('fromByteString' bs) = 'L.fromChunks' [bs]@
--
fromByteString :: S.ByteString -> Builder
fromByteString bs
  | S.null bs = empty
  | otherwise = flush `append` mapBuilder (bs :)

-- | /O(1)./ A Builder taking a lazy 'L.ByteString', satisfying
--
--  * @'toLazyByteString' ('fromLazyByteString' bs) = bs@
--
fromLazyByteString :: L.ByteString -> Builder
fromLazyByteString bss = flush `append` mapBuilder (L.toChunks bss ++)

putString :: String -> Builder
putString = fromLazyByteString . L.pack . map c2w

------------------------------------------------------------------------

-- Our internal buffer type
data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- used bytes
                     {-# UNPACK #-} !Int                -- length left

------------------------------------------------------------------------

-- | /O(n)./ Extract a lazy 'L.ByteString' from a 'Builder'.
-- The construction work takes place if and when the relevant part of
-- the lazy 'L.ByteString' is demanded.
--
toLazyByteString :: Builder -> L.ByteString
toLazyByteString m = L.fromChunks . unsafePerformIO $ do
    buf <- newBuffer defaultSize
    return (runBuilder (m `append` flush) (const []) buf)

-- | /O(1)./ Pop the 'S.ByteString' we have constructed so far, if any,
-- yielding a new chunk in the result lazy 'L.ByteString'.
flush :: Builder
flush = Builder $ \ k buf@(Buffer p o u l) ->
    if u == 0
      then k buf
      else S.PS p o u : k (Buffer p (o+u) 0 l)

------------------------------------------------------------------------

--
-- copied from Data.ByteString.Lazy
--
defaultSize :: Int
defaultSize = 32 * k - overhead
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

------------------------------------------------------------------------

-- | Sequence an IO operation on the buffer
unsafeLiftIO :: (Buffer -> IO Buffer) -> Builder
unsafeLiftIO f =  Builder $ \ k buf -> inlinePerformIO $ do
    buf' <- f buf
    return (k buf')

-- | Get the size of the buffer
withSize :: (Int -> Builder) -> Builder
withSize f = Builder $ \ k buf@(Buffer _ _ _ l) ->
    runBuilder (f l) k buf

-- | Map the resulting list of bytestrings.
mapBuilder :: ([S.ByteString] -> [S.ByteString]) -> Builder
mapBuilder f = Builder (f .)

------------------------------------------------------------------------

-- | Ensure that there are at least @n@ many bytes available.
ensureFree :: Int -> Builder
ensureFree n = n `seq` withSize $ \ l ->
    if n <= l then empty else
        flush `append` unsafeLiftIO (const (newBuffer (max n defaultSize)))

-- | Ensure that @n@ many bytes are available, and then use @f@ to write some
-- bytes into the memory.
writeN :: Int -> (Ptr Word8 -> IO ()) -> Builder
writeN n f = ensureFree n `append` unsafeLiftIO (writeNBuffer n f)

writeNBuffer :: Int -> (Ptr Word8 -> IO ()) -> Buffer -> IO Buffer
writeNBuffer n f (Buffer fp o u l) = do
    withForeignPtr fp (\p -> f (p `plusPtr` (o+u)))
    return (Buffer fp o (u+n) (l-n))

newBuffer :: Int -> IO Buffer
newBuffer size = do
    fp <- S.mallocByteString size
    return $! Buffer fp 0 0 size

------------------------------------------------------------------------
-- Aligned, host order writes of storable values

-- | Ensure that @n@ many bytes are available, and then use @f@ to write some
-- storable values into the memory.
writeNbytes :: Storable a => Int -> (Ptr a -> IO ()) -> Builder
writeNbytes n f = ensureFree n `append` unsafeLiftIO (writeNBufferBytes n f)

writeNBufferBytes :: Storable a => Int -> (Ptr a -> IO ()) -> Buffer -> IO Buffer
writeNBufferBytes n f (Buffer fp o u l) = do
    withForeignPtr fp (\p -> f (p `plusPtr` (o+u)))
    return (Buffer fp o (u+n) (l-n))

------------------------------------------------------------------------

--
-- We rely on the fromIntegral to do the right masking for us.
-- The inlining here is critical, and can be worth 4x performance
--

-- | Write a Word16 in big endian format
putWord16be :: Word16 -> Builder
putWord16be w = writeN 2 $ \p -> do
    poke p               (fromIntegral (shiftR w 8) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (w)              :: Word8)

-- | Write a Word16 in little endian format
putWord16le :: Word16 -> Builder
putWord16le w = writeN 2 $ \p -> do
    poke p               (fromIntegral (w)              :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftR w 8) :: Word8)

-- putWord16le w16 = writeN 2 (\p -> poke (castPtr p) w16)

-- | Write a 24 bit number in big endian format represented as Word32
putWord24be :: Word32 -> Builder
putWord24be w = writeN 3 $ \p -> do
    poke p               (fromIntegral (shiftR w 16) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftR w 8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (w) :: Word8)

-- | Write a 24 bit number in little endian format represented as Word32
putWord24le :: Word32 -> Builder
putWord24le w = writeN 3 $ \p -> do
    poke p               (fromIntegral (w)           :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftR w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftR w 16) :: Word8)

-- | Write a Word32 in big endian format
putWord32be :: Word32 -> Builder
putWord32be w = writeN 4 $ \p -> do
    poke p               (fromIntegral (shiftR w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftR w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftR w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)           :: Word8)

--
-- a data type to tag Put/Check. writes construct these which are then
-- inlined and flattened. matching Checks will be more robust with rules.
--

-- | Write a Word32 in little endian format
putWord32le :: Word32 -> Builder
putWord32le w = writeN 4 $ \p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftR w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftR w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftR w 24) :: Word8)

-- on a little endian machine:
-- putWord32le w32 = writeN 4 (\p -> poke (castPtr p) w32)

-- | Write a Word64 in big endian format
putWord64be :: Word64 -> Builder
putWord64be w = writeN 8 $ \p -> do
    poke p               (fromIntegral (shiftR w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftR w 48) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftR w 40) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftR w 32) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftR w 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftR w 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftR w  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (w)               :: Word8)

-- | Write a Word64 in little endian format
putWord64le :: Word64 -> Builder
putWord64le w = writeN 8 $ \p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftR w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftR w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftR w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftR w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftR w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftR w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftR w 56) :: Word8)

-- on a little endian machine:
-- putWord64le w64 = writeN 8 (\p -> poke (castPtr p) w64)

-----------------------------------------------------------------------

putInt8 :: Int8 -> Builder
putInt8 = putWord8 . fromIntegral

putInt16le :: Int16 -> Builder
putInt16le = putWord16le . fromIntegral

putInt16be :: Int16 -> Builder
putInt16be = putWord16be . fromIntegral

putInt32le :: Int32 -> Builder
putInt32le = putWord32le . fromIntegral

putInt32be :: Int32 -> Builder
putInt32be = putWord32be . fromIntegral

putInt64le :: Int64 -> Builder
putInt64le = putWord64le . fromIntegral

putInt64be :: Int64 -> Builder
putInt64be = putWord64be . fromIntegral

------------------------------------------------------------------------
-- Unaligned, word size ops

-- | /O(1)./ A Builder taking a single native machine word. The word is
-- written in host order, host endian form, for the machine you're on.
-- On a 64 bit machine the Word is an 8 byte value, on a 32 bit machine,
-- 4 bytes. Values written this way are not portable to
-- different endian or word sized machines, without conversion.
--
putWordHost :: Word -> Builder
putWordHost w = writeNbytes (sizeOf (undefined :: Word)) (\p -> poke p w)

-- | Write a Word16 in native host order and host endianness.
-- 2 bytes will be written, unaligned.
putWord16host :: Word16 -> Builder
putWord16host w16 = writeNbytes (sizeOf (undefined :: Word16)) (\p -> poke p w16)

-- | Write a Word32 in native host order and host endianness.
-- 4 bytes will be written, unaligned.
putWord32host :: Word32 -> Builder
putWord32host w32 = writeNbytes (sizeOf (undefined :: Word32)) (\p -> poke p w32)

-- | Write a Word64 in native host order.
-- On a 32 bit machine we write two host order Word32s, in big endian form.
-- 8 bytes will be written, unaligned.
putWord64host :: Word64 -> Builder
putWord64host w = writeNbytes (sizeOf (undefined :: Word64)) (\p -> poke p w)

------------------------------------------------------------------------

putVarLenBe :: Word64 -> Builder
putVarLenBe w = varLenAux2 $ reverse $ varLenAux1 w
  
putVarLenLe :: Word64 -> Builder
putVarLenLe w = varLenAux2 $ varLenAux1 w
  
varLenAux1 :: Word64 -> [Word8]
varLenAux1 0 = []
varLenAux1 w = (fromIntegral $ w .&. 0x7F) : (varLenAux1 $ shiftR w 7)

varLenAux2 :: [Word8] -> Builder
varLenAux2  [] = putWord8 0
varLenAux2  [w] = putWord8 w
varLenAux2 (w : ws) = putWord8 (setBit w 7) `append` varLenAux2 ws