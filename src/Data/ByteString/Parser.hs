-----------------------------------------------------------------------------
-- |
-- Module      : Data.ByteString.Parser
-- Copyright   : Lennart Kolmodin, George Giorgidze
-- License     : BSD3
-- 
-- Maintainer  : George Giorgidze <http://cs.nott.ac.uk/~ggg/>
-- Stability   : experimental
-- Portability : Portable
--
-- The Parser monad. A monad for efficiently building structures from
-- encoded lazy ByteStrings.
--
-----------------------------------------------------------------------------

module Data.ByteString.Parser (

    -- * The Parser type
      Parser
    , runParser
    , runParserState

    -- * Parsing
    , choice
    , expect
    , skip
    , lookAhead
    , lookAheadM
    , lookAheadE

    -- * Utility
    , bytesRead
    , getBytes
    , remaining
    , isEmpty

    -- * Parsing particular types
    , satisfy
    , getString
    , getStringNul
    , string
    , getWord8
    , getInt8
    , word8
    , int8

    -- ** ByteStrings
    , getByteString
    , getLazyByteString
    , getLazyByteStringNul
    , getRemainingLazyByteString

    -- ** Big-endian reads
    , getWord16be
    , word16be
    , getWord24be
    , word24be    
    , getWord32be
    , word32be    
    , getWord64be
    , word64be

    , getInt16be
    , int16be
    , getInt32be
    , int32be    
    , getInt64be
    , int64be

    -- ** Little-endian reads
    , getWord16le
    , word16le
    , getWord24le
    , word24le
    , getWord32le
    , word32le
    , getWord64le
    , word64le

    , getInt16le
    , int16le
    , getInt32le
    , int32le    
    , getInt64le
    , int64le

    -- ** Host-endian, unaligned reads
    , getWordHost
    , wordHost
    , getWord16host
    , word16host
    , getWord32host
    , word32host
    , getWord64host
    , word64host

    -- Variable length reads
    , getVarLenBe
    , varLenBe
    , getVarLenLe
    , varLenLe
    
    -- ** Helper functions
    , char
    , many
    , many1
    , digit
    , count
  ) where

import Control.Monad hiding (join)
import Control.Applicative hiding (many)
import Data.Maybe (isNothing)
import Data.Char (ord)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy.Internal as L

import Foreign

-- used by splitAtST
import Control.Monad.ST
import Data.STRef


-- | The parse state
data S = S {-# UNPACK #-} !B.ByteString  -- current chunk
           L.ByteString                  -- the rest of the input
           {-# UNPACK #-} !Int64         -- bytes read

-- | The Get monad is just a State monad carrying around the input ByteString
newtype Parser a = Parser { unParser :: S -> Either String (a, S) }

instance Functor Parser where
    fmap f m = Parser $ \s -> case unParser m s of
      Left e -> Left e
      Right (a, s') -> Right (f a, s')
    
instance Monad Parser where
    return a  = Parser (\s -> Right (a, s))
    m >>= k   = Parser $ \s -> case unParser m s of
      Left e -> Left e
      Right (a, s') -> unParser (k a) s'
    fail  err  = Parser $ \(S _ _ bytes) -> 
        Left (err ++ ". Failed reading at byte position " ++ show bytes)
instance MonadPlus Parser where
  mzero = Parser $ \_ -> Left []
  mplus p1 p2 = Parser $ \s -> case (unParser p1 s) of
    Left e1 -> case (unParser p2 s) of
      Left e2 -> Left (e1 ++ "\n" ++ e2)
      ok -> ok
    ok -> ok

instance Applicative Parser where
  pure = return
  (<*>) = ap
  
instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

------------------------------------------------------------------------

get :: Parser S
get   = Parser $ \s -> Right (s, s)

put :: S -> Parser ()
put s = Parser $ \_ -> Right ((), s)

------------------------------------------------------------------------

initState :: L.ByteString -> S
initState xs = mkState xs 0

mkState :: L.ByteString -> Int64 -> S
mkState l = case l of
    L.Empty      -> S B.empty L.empty
    L.Chunk x xs -> S x xs

-- | Run the Get monad applies a 'get'-based parser on the input ByteString
runParser :: Parser a -> L.ByteString -> Either String a
runParser m str = case unParser m (initState str) of
  Left e -> Left e
  Right (a, _) -> Right a

-- | Run the Get monad applies a 'get'-based parser on the input
-- ByteString. Additional to the result of get it returns the number of
-- consumed bytes and the rest of the input.
runParserState :: Parser a -> L.ByteString -> Int64 -> Either String (a, L.ByteString, Int64)
runParserState m str off =
    case unParser m (mkState str off) of
      Left e -> Left e
      Right (a, ~(S s ss newOff)) -> Right (a, s `join` ss, newOff)

------------------------------------------------------------------------

choice :: [Parser a] -> Parser a
choice = foldl (<|>) mzero

-- | Skip ahead @n@ bytes. Fails if fewer than @n@ bytes are available.
skip :: Word64 -> Parser ()
skip n = readN (fromIntegral n) (const ())

-- | Run @ga@, but return without consuming its input.
-- Fails if @ga@ fails.
lookAhead :: Parser a -> Parser a
lookAhead ga = do
    s <- get
    a <- ga
    put s
    return a

-- | Like 'lookAhead', but consume the input if @gma@ returns 'Just _'.
-- Fails if @gma@ fails.
lookAheadM :: Parser (Maybe a) -> Parser (Maybe a)
lookAheadM gma = do
    s <- get
    ma <- gma
    when (isNothing ma) $ put s
    return ma

-- | Like 'lookAhead', but consume the input if @gea@ returns 'Right _'.
-- Fails if @gea@ fails.
lookAheadE :: Parser (Either a b) -> Parser (Either a b)
lookAheadE gea = do
    s <- get
    ea <- gea
    case ea of
        Left _ -> put s
        _      -> return ()
    return ea

expect :: (Show a, Eq a) => (a -> Bool) -> Parser a -> Parser a
expect f p = do
  v <- p
  unless (f v) $ fail $ show v ++ " was not expected."
  return v

getString :: Int -> Parser String
getString l = do
  bs <- getLazyByteString (fromIntegral l)
  return $! map B.w2c (L.unpack bs)

getStringNul :: Parser String
getStringNul = do
  bs <- getLazyByteStringNul
  return $! map B.w2c (L.unpack bs)

string :: String -> Parser String
string s = expect (s ==) (getString $ length s)

-- Utility

-- | Get the total number of bytes read to this point.
bytesRead :: Parser Int64
bytesRead = do
    S _ _ b <- get
    return b

-- | Get the number of remaining unparsed bytes.
-- Useful for checking whether all input has been consumed.
-- Note that this forces the rest of the input.
remaining :: Parser Int64
remaining = do
    S s ss _ <- get
    return $! (fromIntegral (B.length s) + L.length ss)

-- | Test whether all input has been consumed,
-- i.e. there are no remaining unparsed bytes.
isEmpty :: Parser Bool
isEmpty = do
    S s ss _ <- get
    return $! (B.null s && L.null ss)

------------------------------------------------------------------------
-- Utility with ByteStrings

-- | An efficient 'get' method for strict ByteStrings. Fails if fewer
-- than @n@ bytes are left in the input.
getByteString :: Int -> Parser B.ByteString
getByteString n = readN n id

-- | An efficient 'get' method for lazy ByteStrings. Does not fail if fewer than
-- @n@ bytes are left in the input.
getLazyByteString :: Int64 -> Parser L.ByteString
getLazyByteString n = do
    S s ss bytes <- get
    let big = s `join` ss
    case splitAtST n big of
      (consume, rest) -> do put $ mkState rest (bytes + n)
                            return consume

-- | Get a lazy ByteString that is terminated with a NUL byte. Fails
-- if it reaches the end of input without hitting a NUL.
getLazyByteStringNul :: Parser L.ByteString
getLazyByteStringNul = do
    S s ss bytes <- get
    let big = s `join` ss
        (consume, t) = L.break (== 0) big
        (h, rest) = L.splitAt 1 t
    when (L.null h) $ fail "too few bytes"
    put $ mkState rest (bytes + L.length consume + 1)
    return consume

-- | Get the remaining bytes as a lazy ByteString
getRemainingLazyByteString :: Parser L.ByteString
getRemainingLazyByteString = do
    S s ss _ <- get
    return $! (s `join` ss)

------------------------------------------------------------------------
-- Helpers

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytes :: Int -> Parser B.ByteString
getBytes n = do
    S s ss bytes <- get
    if n <= B.length s
        then do let (consume,rest) = B.splitAt n s
                put $! S rest ss (bytes + fromIntegral n)
                return $! consume
        else
              case L.splitAt (fromIntegral n) (s `join` ss) of
                (consuming, rest) ->
                    do let now = B.concat . L.toChunks $ consuming
                       put $! mkState rest (bytes + fromIntegral n)
                       -- forces the next chunk before this one is returned
                       when (B.length now < n) $ fail "too few bytes"
                       return now


join :: B.ByteString -> L.ByteString -> L.ByteString
join bb lb
    | B.null bb = lb
    | otherwise = L.Chunk bb lb

-- | Split a ByteString. If the first result is consumed before the --
-- second, this runs in constant heap space.
--
-- You must force the returned tuple for that to work, e.g.
-- 
-- > case splitAtST n xs of
-- >    (ys,zs) -> consume ys ... consume zs
--
splitAtST :: Int64 -> L.ByteString -> (L.ByteString, L.ByteString)
splitAtST i ps | i <= 0 = (L.empty, ps)
splitAtST i ps          = runST (
     do r  <- newSTRef undefined
        xs <- first r i ps
        ys <- unsafeInterleaveST (readSTRef r)
        return (xs, ys))

  where
        first r 0 xs@(L.Chunk _ _) = writeSTRef r xs    >> return L.Empty
        first r _ L.Empty          = writeSTRef r L.Empty >> return L.Empty

        first r n (L.Chunk x xs)
          | n < l     = do writeSTRef r (L.Chunk (B.drop (fromIntegral n) x) xs)
                           return $! L.Chunk (B.take (fromIntegral n) x) L.Empty
          | otherwise = do writeSTRef r (L.drop (n - l) xs)
                           liftM (L.Chunk x) $ unsafeInterleaveST (first r (n - l) xs)

         where l = fromIntegral (B.length x)

-- Pull n bytes from the input, and apply a parser to those bytes,
-- yielding a value. If less than @n@ bytes are available, fail with an
-- error. This wraps @getBytes@.
readN :: Int -> (B.ByteString -> a) -> Parser a
readN n f = fmap f $ getBytes n


------------------------------------------------------------------------
-- Primtives

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying lazy byteString. So many indirections from the raw parser
-- state that my head hurts...

getPtr :: Storable a => Int -> Parser a
getPtr n = do
    (fp,o,_) <- readN n B.toForeignPtr
    return . B.inlinePerformIO $ withForeignPtr fp $ peek . castPtr . (`plusPtr` o)

------------------------------------------------------------------------

satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy f = do
  w <- getWord8
  guard (f w)
  return w

-- | Read a Word8 from the monad state
getWord8 :: Parser Word8
getWord8 = getPtr (sizeOf (undefined :: Word8))

word8 :: Word8 -> Parser Word8
word8 w = expect (w ==) getWord8

-- | Read a Word16 in big endian format
getWord16be :: Parser Word16
getWord16be = do
    s <- readN 2 id
    return $! (fromIntegral (s `B.index` 0) `shiftL` 8) .|.
               fromIntegral (s `B.index` 1)

word16be :: Word16 -> Parser Word16
word16be w = expect (w ==) getWord16be

-- | Read a Word16 in little endian format
getWord16le :: Parser Word16
getWord16le = do
    s <- readN 2 id
    return $! (fromIntegral (s `B.index` 1) `shiftL` 8) .|.
               fromIntegral (s `B.index` 0)

word16le :: Word16 -> Parser Word16
word16le w = expect (w ==) getWord16le

-- | Read a 24 bit word into Word32 in big endian format
getWord24be :: Parser Word32
getWord24be = do
    s <- readN 3 id
    return $! (fromIntegral (s `B.index` 0) `shiftL` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftL`  8) .|.
               fromIntegral (s `B.index` 2)

word24be :: Word32 -> Parser Word32
word24be w = expect (w ==) getWord24be

getWord24le :: Parser Word32
getWord24le = do
    s <- readN 3 id
    return $! (fromIntegral (s `B.index` 2) `shiftL` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftL`  8) .|.
               fromIntegral (s `B.index` 0)

word24le :: Word32 -> Parser Word32
word24le w = expect (w ==) getWord24le

-- | Read a Word32 in big endian format
getWord32be :: Parser Word32
getWord32be = do
    s <- readN 4 id
    return $! (fromIntegral (s `B.index` 0) `shiftL` 24) .|.
              (fromIntegral (s `B.index` 1) `shiftL` 16) .|.
              (fromIntegral (s `B.index` 2) `shiftL`  8) .|.
               fromIntegral (s `B.index` 3)

word32be :: Word32 -> Parser Word32
word32be w = expect (w ==) getWord32be

-- | Read a Word32 in little endian format
getWord32le :: Parser Word32
getWord32le = do
    s <- readN 4 id
    return $! (fromIntegral (s `B.index` 3) `shiftL` 24) .|.
              (fromIntegral (s `B.index` 2) `shiftL` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftL`  8) .|.
               fromIntegral (s `B.index` 0)

word32le :: Word32 -> Parser Word32
word32le w = expect (w ==) getWord32le


-- | Read a Word64 in big endian format
getWord64be :: Parser Word64
getWord64be = do
    s <- readN 8 id
    return $! (fromIntegral (s `B.index` 0) `shiftL` 56) .|.
              (fromIntegral (s `B.index` 1) `shiftL` 48) .|.
              (fromIntegral (s `B.index` 2) `shiftL` 40) .|.
              (fromIntegral (s `B.index` 3) `shiftL` 32) .|.
              (fromIntegral (s `B.index` 4) `shiftL` 24) .|.
              (fromIntegral (s `B.index` 5) `shiftL` 16) .|.
              (fromIntegral (s `B.index` 6) `shiftL`  8) .|.
               fromIntegral (s `B.index` 7)

word64be :: Word64 -> Parser Word64
word64be w = expect (w ==) getWord64be

-- | Read a Word64 in little endian format
getWord64le :: Parser Word64
getWord64le = do
    s <- readN 8 id
    return $! (fromIntegral (s `B.index` 7) `shiftL` 56) .|.
              (fromIntegral (s `B.index` 6) `shiftL` 48) .|.
              (fromIntegral (s `B.index` 5) `shiftL` 40) .|.
              (fromIntegral (s `B.index` 4) `shiftL` 32) .|.
              (fromIntegral (s `B.index` 3) `shiftL` 24) .|.
              (fromIntegral (s `B.index` 2) `shiftL` 16) .|.
              (fromIntegral (s `B.index` 1) `shiftL`  8) .|.
               fromIntegral (s `B.index` 0)

word64le :: Word64 -> Parser Word64
word64le w = expect (w ==) getWord64le
------------------------------------------------------------------------
getInt8 :: Parser Int8
getInt8 = fmap fromIntegral getWord8

int8 :: Int8 -> Parser Int8
int8 i = expect (i ==) getInt8

getInt16le :: Parser Int16
getInt16le = fmap fromIntegral getWord16le

int16le :: Int16 -> Parser Int16
int16le i = expect (i ==) getInt16le

getInt16be :: Parser Int16
getInt16be = fmap fromIntegral getWord16be

int16be :: Int16 -> Parser Int16
int16be i = expect (i ==) getInt16be

getInt32le :: Parser Int32
getInt32le = fmap fromIntegral getWord32le

int32le :: Int32 -> Parser Int32
int32le i = expect (i ==) getInt32le

getInt32be :: Parser Int32
getInt32be = fmap fromIntegral getWord32be

int32be :: Int32 -> Parser Int32
int32be i = expect (i ==) getInt32be

getInt64le :: Parser Int64
getInt64le = fmap fromIntegral getWord64le

int64le :: Int64 -> Parser Int64
int64le i = expect (i ==) getInt64le

getInt64be :: Parser Int64
getInt64be = fmap fromIntegral getWord64be

int64be :: Int64 -> Parser Int64
int64be i = expect (i ==) getInt64be

------------------------------------------------------------------------
-- Host-endian reads

-- | /O(1)./ Read a single native machine word. The word is read in
-- host order, host endian form, for the machine you're on. On a 64 bit
-- machine the Word is an 8 byte value, on a 32 bit machine, 4 bytes.
getWordHost :: Parser Word
getWordHost = getPtr (sizeOf (undefined :: Word))

wordHost :: Word -> Parser Word
wordHost w = expect (w ==) getWordHost

-- | /O(1)./ Read a 2 byte Word16 in native host order and host endianness.
getWord16host :: Parser Word16
getWord16host = getPtr (sizeOf (undefined :: Word16))

word16host :: Word16 -> Parser Word16
word16host w = expect (w ==) getWord16host

-- | /O(1)./ Read a Word32 in native host order and host endianness.
getWord32host :: Parser Word32
getWord32host = getPtr  (sizeOf (undefined :: Word32))

word32host :: Word32 -> Parser Word32
word32host w = expect (w ==) getWord32host

-- | /O(1)./ Read a Word64 in native host order and host endianess.
getWord64host   :: Parser Word64
getWord64host = getPtr  (sizeOf (undefined :: Word64))

word64host :: Word64 -> Parser Word64
word64host w = expect (w ==) getWord64host

-- Variable length numbers

getVarLenBe :: Parser Word64
getVarLenBe = f 0
  where
  f :: Word64 -> Parser Word64
  f acc =  do
    w <- fmap fromIntegral getWord8
    if testBit w 7
      then f      $! shiftL acc 7 .|. clearBit w 7
      else return $! shiftL acc 7 .|. w

varLenBe :: Word64 -> Parser Word64
varLenBe a = expect (a ==) getVarLenBe

getVarLenLe :: Parser Word64
getVarLenLe = do
  w <- fmap fromIntegral getWord8
  if testBit w 7
    then do
      w' <- getVarLenLe
      return $! clearBit w 7 .|. shiftL w' 7
    else return $! w

varLenLe :: Word64 -> Parser Word64
varLenLe a = expect (a ==) getVarLenLe

---------------------------------------------------------
-- Helper functions
---------------------------------------------------------

-- anyByte :: Parser Word8
-- anyByte = word8

-- | Convenience function to let you parse the equivalent Word8 for a given Char
char :: Char -> Parser Word8
char = word8 . fromIntegral . ord

-- | Find zero or more elements matching a given parser
many :: Parser a -> Parser [a]
many p = many1 p `mplus` return []

-- | Find one or more elements matching a given parser
many1 :: Parser a -> Parser [a]
many1 p = do
            x <- p
            xs <- many p
            return (x:xs)

-- | matches on any equivalent Word8 correcponding to @['0'..'9']@
digit :: Parser Word8
digit = choice . map char $ ['0'..'9']

-- | Attempts to match the parser @p@ @n@ times
count :: Int -> Parser a -> Parser [a]
count n p | n <= 0 = mzero
count n p = (:) <$> p <*> count (n-1) p