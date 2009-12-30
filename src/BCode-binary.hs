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
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Word
import Data.Int
import Data.Digest.Pure.SHA
import Data.List
import Data.Maybe
import qualified Data.Map as M
-- import Text.ParserCombinators.Parsec
import Text.PrettyPrint.HughesPJ hiding (char)

-- import Data.ByteString.Parser
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get



data BCode = BInt Integer
           | BString B.ByteString
           | BArray [BCode]
           | BDict (M.Map B.ByteString BCode)
  deriving Show

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

-- wrap :: Char -> Char -> B.ByteString -> B.ByteString
-- wrap b e = B.cons (toW8 b) . flip B.snoc (toW8 e)


-- encode :: BCode -> B.ByteString
-- -- encode (BInt i) = toBS $ "i" ++ show i ++ "e"
-- encode (BInt i) = wrap 'i' 'e' . toBS . show $ i
-- encode (BString s) = toBS (show (B.length s)) `B.append` B.cons (toW8 ':') s
-- encode (BArray arr) =  B.cons (toW8 'l') $ (B.concat . map encode $ arr) `B.append` toBS "e"
-- -- encode (BDict mp) = "d" ++ concatMap encPair (M.toList mp) ++ "e"
-- encode (BDict mp) = wrap 'd' 'e' dict
--     where encPair (k, v) = encode (BString k) `B.append` encode v
--           dict = B.concat . map encPair . M.toList $ mp

instance Binary BCode where
    put (BInt i)     = wrap 'i' 'e' $ putShow i
    put (BString s)  = do
                         putShow (B.length s)
                         putWord8 (toW8 ':')
                         putLazyByteString s
    put (BArray arr) = wrap 'l' 'e' . mapM_ put $ arr
    put (BDict mp)   = wrap 'd' 'e' dict
                     where dict = mapM_ encPair . M.toList $ mp
                           encPair (k, v) = put (BString k) >> put v
    
    get = do
            x <- lookAhead getWord8
            case x of
                 n | fromW8 n == 'i'    -> getBInt
                   | fromW8 n == 'l'    -> getBArray
                   | fromW8 n == 'd'    -> getBDict
                   | isDigit (fromW8 n) -> getBString
                 n -> fail $ "Unexpected character: " ++ [fromW8 n]

-- parseInt :: Parser BCode
-- parseInt = do
--   char 'i'
--   i <- many1 digit
--   char 'e'
--   return . BInt . read . map fromW8 $ i

getBInt :: Get BCode
getBInt =
    do
        char 'i'
        str <- getDigits
        char 'e'
        return . BInt . read $ str


-- parseString :: Parser BCode
-- parseString = do
--   n <- many1 digit
--   let i = read (map fromW8 n) :: Int64
--   char ':'
--   s <- getLazyByteString i
--   return $ BString s

getBString :: Get BCode
getBString =
    do
        count <- getDigits
        char ':'
        BString <$> getLazyByteString (read count)


-- parseList :: Parser BCode
-- parseList = do
--   char 'l'
--   l <- many parseBCode
--   char 'e'
--   return $ BArray l
        
getBArray :: Get BCode
getBArray = 
    do
        char 'l'
        l <- getBArrayContents
        return . BArray $ l

getBArrayContents =
    do
        x <- lookAhead getCharG
        if x == 'e'
            then return []
            else (:) <$> get <*> getBArrayContents

-- parseDict :: Parser BCode
-- parseDict = do char 'd'
--                l <- many parsePair
--                char 'e'
--                return . BDict . M.fromList $ l
--   where parsePair = do
--           (BString s) <- parseString
--           b <- parseBCode
--           return (s,b)
        
getBDict :: Get BCode
getBDict = do
    char 'd'
    contents <- getBDictContents
    return . BDict . M.fromList $ contents

getBDictContents :: Get [(B.ByteString,BCode)]
getBDictContents = do
    x <- lookAhead getCharG
    if x == 'e'
        then return []
        else do
                (BString s) <- getBString
                x <- get
                ((s,x):) <$> getBDictContents


getUntilE :: Get a -> Get [a]
getUntilE p =
    do
        x <- lookAhead getCharG
        if x == 'e'
            then return []
            else (:) <$> p <*> getUntilE p

getDigits :: Get String
getDigits = manyG1 isDigit getCharG

-- Helper functions

manyG :: Binary a => (a -> Bool) -> Get a -> Get [a]
manyG b g = 
    do
        x <- lookAhead g
        if b x
            then (:) <$> g <*> manyG b g
            else return []

manyG1 :: Binary a => (a -> Bool) -> Get a -> Get [a]
manyG1 b p =
    do
        x <- p
        if b x
            then (x:) <$> manyG b p
            else return [] 

char :: Char -> Get ()
char c = 
    do
        x <- getWord8
        unless (fromW8 x == c) $
            fail ("Expected char: '" ++ c:"' got: '" ++ [fromW8 x,'\''])

getCharG :: Get Char
getCharG = fromW8 <$> getWord8

wrap :: Char -> Char -> Put -> Put
wrap a b m = do
                putWord8 (toW8 a)
                m
                putWord8 (toW8 b)

putShow :: Show a => a -> Put
putShow = mapM_ put . show

-- encodeBS :: BCode -> B.ByteString
-- encodeBS = B.pack . map (fromIntegral . ord) . encode

-- | Return the hash of the info-dict in a torrent file
hashInfoDict :: BCode -> Maybe B.ByteString
hashInfoDict bc =
    do ih <- info bc
       let encoded = encode ih
       return $ bytestringDigest $ sha1 encoded



-- parseDict :: Parser BCode
-- parseDict = do char 'd'
--                l <- many parsePair
--                char 'e'
--                return . BDict . M.fromList $ l
--   where parsePair = do
--           (BString s) <- parseString
--           b <- parseBCode
--           return (s,b)
-- 
-- parseBCode :: Parser BCode
-- parseBCode = parseString `mplus` parseList `mplus` parseInt `mplus` parseDict

-- Use parsec for this bastard
-- decode :: String -> Either ParseError BCode
-- decode = parse parseBCode "(unknown)"

-- decode :: B.ByteString -> Either String BCode
-- decode = runParser parseBCode


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
                    BString str -> return $ sha1Split str --(B.pack $ map (fromIntegral . ord) str)
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
      BString s -> text (fromBS s)
      BArray arr -> text "[" <+> cat (intersperse comma al) <+> text "]"
          where al = map pp arr
      BDict mp -> text "{" <+> cat (intersperse comma mpl) <+> text "}"
          where mpl = map (\(s, bc') -> text (fromBS s) <+> text "->" <+> pp bc') $ M.toList mp

prettyPrint :: BCode -> String
prettyPrint = render . pp