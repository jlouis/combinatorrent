-- Haskell Torrent
-- Copyright (c) 2009, Jesper Louis Andersen,
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--  * Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--  * Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


-- | Add a module description here
--   also add descriptions to each function.
module BCode 
(
              BCode,
              Path(..),
              encode,
              -- encodeBS,
              decode,
              search,
              announce,
              comment,
              creationDate,
              info,
              hashInfoDict,
              infoLength,
              infoName,
              infoPieceLength,
              infoPieces,
              numberPieces,
              prettyPrint,

              trackerComplete,
              trackerIncomplete,
              trackerInterval,
              trackerMinInterval,
              trackerPeers,
              trackerWarning,
              trackerError,
              toBS,
              fromBS )

where

import Control.Monad
import Control.Applicative hiding (many)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Char
import Data.Word
import Data.Int
import Data.Digest.Pure.SHA
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ hiding (char)

import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get


-- | BCode represents the structure of a bencoded file
data BCode = BInt Integer                       -- ^ An integer
           | BString B.ByteString               -- ^ A string of bytes
           | BArray [BCode]                     -- ^ An array
           | BDict (M.Map B.ByteString BCode)   -- ^ A key, value map
  deriving (Show, Eq)

data Path = PString B.ByteString
          | PInt Int

toW8 :: Char -> Word8
toW8 = fromIntegral . ord

fromW8 :: Word8 -> Char
fromW8 = chr . fromIntegral

toBS :: String -> B.ByteString
toBS = B.pack . map toW8

fromBS :: B.ByteString -> String
fromBS = map fromW8 . B.unpack


instance Serialize BCode where
    put (BInt i)     = wrap 'i' 'e' $ putShow i
    put (BArray arr) = wrap 'l' 'e' . mapM_ put $ arr
    put (BDict mp)   = wrap 'd' 'e' dict
                     where dict = mapM_ encPair . M.toList $ mp
                           encPair (k, v) = put (BString k) >> put v
    put (BString s)  = do
                         putShow (B.length s)
                         putWord8 (toW8 ':')
                         putByteString s
    
    get = getBInt <|> getBArray <|> getBDict <|> getBString

-- | Get something wrapped in two Chars
getWrapped :: Char -> Char -> Get a -> Get a
getWrapped a b p = char a *> p <* char b

-- | Parses a BInt
getBInt :: Get BCode
getBInt = BInt . read <$> getWrapped 'i' 'e' intP
    where intP = ((:) <$> char '-' <*> getDigits) <|> getDigits

-- | Parses a BArray
getBArray :: Get BCode
getBArray = BArray <$> getWrapped 'l' 'e' (many get)


-- | Parses a BDict
getBDict :: Get BCode
getBDict = BDict . M.fromList <$> getWrapped 'd' 'e' (many getPairs)
    where getPairs = do
            (BString s) <- getBString
            x <- get
            return (s,x)

-- | Parses a BString
getBString :: Get BCode
getBString = do
    count <- getDigits
    BString <$> ( char ':' *> getStr (read count :: Integer))
    where maxInt = fromIntegral (maxBound :: Int) :: Integer
          
          getStr n | n >= 0 = B.concat <$> (sequence $ getStr' n)
                   | otherwise = fail $ "read a negative length string, length: " ++ show n
          
          getStr' n | n > maxInt = getByteString maxBound : getStr' (n-maxInt)
                    | otherwise = [getByteString . fromIntegral $ n]


-- | Get one or more digit characters
getDigits :: Get String
getDigits = many1 digit

-- | Returns a character if it is a digit, fails otherwise. uses isDigit.
digit :: Get Char
digit = do
    x <- getCharG
    if isDigit x
        then return x
        else fail $ "Expected digit, got: " ++ show x


-- * Put helper functions

-- | Put an element, wrapped by two characters
wrap :: Char -> Char -> Put -> Put
wrap a b m = do
    putWord8 (toW8 a)
    m
    putWord8 (toW8 b)

-- | Put something as it is shown using @show@
putShow :: Show a => a -> Put
putShow x = mapM_ put (show x)

-- * Get Helper functions

-- | Parse zero or items using a given parser
many :: Get a -> Get [a]
many p = many1 p `mplus` return []

-- | Parse one or more items using a given parser
many1 :: Get a -> Get [a]
many1 p = (:) <$> p <*> many p

-- | Parse a given character
char :: Char -> Get Char
char c = do
    x <- getCharG
    if x == c
        then return c
        else fail $ "Expected char: '" ++ c:"' got: '" ++ [x,'\'']

-- | Get a Char. Only works with single byte characters
getCharG :: Get Char
getCharG = fromW8 <$> getWord8



-- BCode helper functions

-- | Return the hash of the info-dict in a torrent file
hashInfoDict :: BCode -> Maybe L.ByteString
hashInfoDict bc =
    do ih <- info bc
       let encoded = encode ih
       return . bytestringDigest . sha1 . L.fromChunks $ [encoded]


toPS :: String -> Path
toPS = PString . toBS

{- Simple search function over BCoded data structures, general case. In practice, we
   will prefer some simpler mnemonics -}
search :: [Path] -> BCode -> Maybe BCode
search [] bc = Just bc
search (PInt i : rest) (BArray bs) | i < 0 || i > length bs = Nothing
                                   | otherwise = search rest (bs!!i)
search (PString s : rest) (BDict mp) = M.lookup s mp >>= search rest
search _ _ = Nothing

search' :: String -> BCode -> Maybe B.ByteString
search' str b = case search [toPS str] b of
                  Nothing -> Nothing
                  Just (BString s) -> Just s
                  _ -> Nothing

searchStr :: String -> BCode -> Maybe B.ByteString
searchStr = search'

searchInt :: String -> BCode -> Maybe Integer
searchInt str b = case search [toPS str] b of
                    Just (BInt i) -> Just i
                    _ -> Nothing

searchInfo :: String -> BCode -> Maybe BCode
searchInfo str = search [toPS "info", toPS str]

{- Various accessors -}
announce, comment, creationDate :: BCode -> Maybe B.ByteString
announce = search' "announce"
comment  = search' "comment"
creationDate = search' "creation date"

{- Tracker accessors -}
trackerComplete, trackerIncomplete, trackerInterval :: BCode -> Maybe Integer
trackerMinInterval :: BCode -> Maybe Integer
trackerComplete = searchInt "complete"
trackerIncomplete = searchInt "incomplete"
trackerInterval = searchInt "interval"
trackerMinInterval = searchInt "min interval"

trackerError, trackerWarning :: BCode -> Maybe B.ByteString
trackerError = searchStr "failure reason"
trackerWarning = searchStr "warning mesage"

trackerPeers :: BCode -> Maybe B.ByteString
trackerPeers = searchStr "peers"

info :: BCode -> Maybe BCode
info = search [toPS "info"]

infoName :: BCode -> Maybe B.ByteString
infoName bc = case search [toPS "info", toPS "name"] bc of
               Just (BString s) -> Just s
               _ -> Nothing

infoPieceLength ::BCode -> Maybe Integer
infoPieceLength bc = do BInt i <- search [toPS "info", toPS "piece length"] bc
                        return i

infoLength :: BCode -> Maybe Integer
infoLength bc = do BInt i <- search [toPS "info", toPS "length"] bc
                   return i

infoPieces :: BCode -> Maybe [B.ByteString]
infoPieces b = do t <- searchInfo "pieces" b
                  case t of
                    BString str -> return $ sha1Split str
                    _ -> mzero
      where sha1Split r | r == B.empty = []
                        | otherwise = block : sha1Split rest
                            where (block, rest) = B.splitAt 20 r

numberPieces :: BCode -> Maybe Int
numberPieces = fmap length . infoPieces

pp :: BCode -> Doc
pp bc =
    case bc of
      BInt i -> integer i
      BString s -> text (show s)
      BArray arr -> text "[" <+> (cat $ intersperse comma al) <+> text "]"
          where al = map pp arr
      BDict mp -> text "{" <+> (cat $ intersperse comma mpl) <+> text "}"
          where mpl = map (\(s, bc') -> text (fromBS s) <+> text "->" <+> pp bc') $ M.toList mp

prettyPrint :: BCode -> String
prettyPrint = render . pp


testDecodeEncodeProp1 :: BCode -> Bool
testDecodeEncodeProp1 m =
    let encoded = encode m
        decoded = decode encoded
    in case decoded of
         Left _ -> False
         Right m' -> m == m'

testData = [BInt 123,
            BInt (-123),
            BString (toBS "Hello"),
            BString (toBS ['\NUL'..'\255']),
            BArray [BInt 1234567890
                   ,toBString "a longer string with eieldei stuff to mess things up"
                   ],
            toBDict [
                     ("hello",BInt 3)
                    ,("a key",toBString "and a value")
                    ,("a sub dict",toBDict [
                                            ("some stuff",BInt 1)
                                           ,("some more stuff", toBString "with a string")
                                           ])
                    ]
           ]

toBDict :: [(String,BCode)] -> BCode
toBDict = BDict . M.fromList . map (\(k,v) -> ((toBS k),v))

toBString :: String -> BCode
toBString = BString . toBS




