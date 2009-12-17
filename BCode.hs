-- | Add a copyright header and module description here
--   also add descriptions to each function.
module BCode (
              BCode,
              Path(..),
              encode,
              encodeBS,
              decode,
              search,
              announce,
              comment,
              creationDate,
              info,
              hashInfoDict,
              infoLength,
              infoName,
              infoNameUtf8,
              infoPieceLength,
              infoPieces,
              prettyPrint,

              trackerComplete,
              trackerIncomplete,
              trackerInterval,
              trackerMinInterval,
              trackerPeers,
              trackerWarning,
              trackerError )

where

import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Digest.Pure.SHA
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Text.PrettyPrint.HughesPJ hiding (char)


data BCode = BInt Integer
           | BString String
           | BArray [BCode]
           | BDict (M.Map String BCode)
  deriving Show

data Path = PString String
          | PInt Int

encode :: BCode -> String
encode (BInt i) = "i" ++ show i ++ "e"
encode (BString s) = show (length s) ++ ":" ++ s
encode (BArray arr) = "l" ++ concatMap encode arr ++ "e"
encode (BDict mp) = "d" ++ concatMap encPair (M.toList mp) ++ "e"
    where encPair (k, v) = encode (BString k) ++ encode v

encodeBS :: BCode -> B.ByteString
encodeBS = B.pack . map (fromIntegral . ord) . encode

-- | Return the hash of the info-dict in a torrent file
hashInfoDict :: BCode -> Maybe String
hashInfoDict bc =
    do ih <- info bc
       let encoded = encodeBS ih
       return $ showDigest $ sha1 encoded

parseInt :: GenParser Char st BCode
parseInt = do
  char 'i'
  i <- many1 digit
  char 'e'
  return $ BInt $ read i

parseString :: GenParser Char st BCode
parseString = do
  n <- many1 digit
  let i = read n :: Int
  char ':'
  s <- count i anyChar
  return $ BString s

parseList :: GenParser Char st BCode
parseList = do
  char 'l'
  l <- many parseBCode
  char 'e'
  return $ BArray l

parseDict :: GenParser Char st BCode
parseDict = do char 'd'
               l <- many parsePair
               char 'e'
               return $ BDict $ M.fromList l
  where parsePair = do
          (BString s) <- parseString
          b <- parseBCode
          return (s,b)

parseBCode :: GenParser Char st BCode
parseBCode = parseString <|> parseList <|> parseInt <|> parseDict

-- Use parsec for this bastard
decode :: String -> Either ParseError BCode
decode = parse parseBCode "(unknown)"


{- Simple search function over BCoded data structures, general case. In practice, we
   will prefer some simpler mnemonics -}
search :: [Path] -> BCode -> Maybe BCode
search [] bc = Just bc
search (PInt i : rest) (BArray bs) | i < 0 || i > length bs = Nothing
                                   | otherwise = search rest (bs!!i)
search (PString s : rest) (BDict mp) = M.lookup s mp >>= search rest
search _ _ = Nothing

search' :: String -> BCode -> Maybe String
search' str b = case search [PString str] b of
                  Nothing -> Nothing
                  Just (BString s) -> Just s
                  _ -> Nothing

searchStr :: String -> BCode -> Maybe String
searchStr = search'

searchInt :: String -> BCode -> Maybe Integer
searchInt str = fmap read . search' str

searchInfo :: String -> BCode -> Maybe BCode
searchInfo str = search [PString "info", PString str]

{- Various accessors -}
announce, comment, creationDate :: BCode -> Maybe String
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

trackerError, trackerWarning :: BCode -> Maybe String
trackerError = searchStr "failure reason"
trackerWarning = searchStr "warning mesage"

trackerPeers :: BCode -> Maybe String
trackerPeers = searchStr "peers"

info :: BCode -> Maybe BCode
info = search [PString "info"]

infoName, infoNameUtf8 :: BCode -> Maybe BCode
infoName   = searchInfo "name"
infoNameUtf8 = searchInfo "name.utf-8"

infoPieceLength ::BCode -> Maybe Integer
infoPieceLength bc = do BInt i <- search [PString "info", PString "piece length"] bc
                        return i

infoLength :: BCode -> Maybe Integer
infoLength bc = do BInt i <- search [PString "info", PString "length"] bc
                   return i

infoPieces :: BCode -> Maybe [String]
infoPieces b = do t <- searchInfo "pieces" b
                  case t of
                    BString str -> return $ sha1Split str
                    _ -> mzero
      where sha1Split "" = []
            sha1Split r  = block : sha1Split rest
                where (block, rest) = splitAt 20 r

pp :: BCode -> Doc
pp bc =
    case bc of
      BInt i -> integer i
      BString s -> text s
      BArray arr -> cat $ intersperse comma al
          where al = map pp arr
      BDict mp -> cat $ intersperse comma mpl
          where mpl = map (\(s, bc') -> text s <+> text "->" <+> pp bc') $ M.toList mp

prettyPrint :: BCode -> String
prettyPrint = render . pp