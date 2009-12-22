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
module BCode (
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

import Data.ByteString.Parser



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

encode :: BCode -> B.ByteString
encode (BInt i) = toBS $ "i" ++ show i ++ "e"
encode (BString s) = toBS (show (B.length s)) `B.append` B.cons (toW8 ':') s
-- encode (BArray arr) =  cons (toW8 'l') $ concatMap encode arr ++ "e"
-- encode (BDict mp) = "d" ++ concatMap encPair (M.toList mp) ++ "e"
--     where encPair (k, v) = encode (BString k) ++ encode v

-- encodeBS :: BCode -> B.ByteString
-- encodeBS = B.pack . map (fromIntegral . ord) . encode

-- | Return the hash of the info-dict in a torrent file
hashInfoDict :: BCode -> Maybe B.ByteString
hashInfoDict bc =
    do ih <- info bc
       let encoded = encode ih
       return $ bytestringDigest $ sha1 encoded


parseInt :: Parser BCode
parseInt = do
  char 'i'
  i <- many1 digit
  char 'e'
  return . BInt . read . map fromW8 $ i

parseString :: Parser BCode
parseString = do
  n <- many1 digit
  let i = read (map fromW8 n) :: Int64
  char ':'
  s <- getLazyByteString i
  return $ BString s

parseList :: Parser BCode
parseList = do
  char 'l'
  l <- many parseBCode
  char 'e'
  return $ BArray l

parseDict :: Parser BCode
parseDict = do char 'd'
               l <- many parsePair
               char 'e'
               return $ BDict $ M.fromList l
  where parsePair = do
          (BString s) <- parseString
          b <- parseBCode
          return (s,b)

parseBCode :: Parser BCode
parseBCode = parseString `mplus` parseList `mplus` parseInt `mplus` parseDict

-- Use parsec for this bastard
-- decode :: String -> Either ParseError BCode
-- decode = parse parseBCode "(unknown)"

decode :: B.ByteString -> Either String BCode
decode = runParser parseBCode


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
      BArray arr -> cat $ intersperse comma al
          where al = map pp arr
      BDict mp -> cat $ intersperse comma mpl
          where mpl = map (\(s, bc') -> text (fromBS s) <+> text "->" <+> pp bc') $ M.toList mp

prettyPrint :: BCode -> String
prettyPrint = render . pp