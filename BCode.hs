module BCode (
              BCode,
              Path(..),
              encode,
              decode,
              search,
              announce,
              comment,
              creationDate,
              info,
              infoLength,
              infoName,
              infoNameUtf8,
              infoPieceLength,
              infoPieces,
              prettyPrint,

              tracker_complete,
              tracker_incomplete,
              tracker_interval,
              tracker_min_interval,
              tracker_peers,
              tracker_warning,
              tracker_error )

where

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Text.PrettyPrint.HughesPJ hiding (char)


data BCode = BInt Int
           | BString String
           | BArray [BCode]
           | BDict (M.Map String BCode)
  deriving Show

data Path = PString String
          | PInt Int

encode :: BCode -> String
encode (BInt i) = "i" ++ show i ++ "e"
encode (BString s) = show (length s) ++ ":" ++ show s -- This isn't right yet, I think
encode (BArray arr) = "l" ++ concatMap encode arr ++ "e"
encode (BDict mp) = "d" ++ concatMap encPair (M.toList mp) ++ "e"
    where encPair (k, v) = encode (BString k) ++ encode v


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
searchInt str b = fmap read $ search' str b

searchInfo :: String -> BCode -> Maybe BCode
searchInfo str = search [PString "info", PString str]

{- Various accessors -}
announce, comment, creationDate :: BCode -> Maybe String
announce = search' "announce"
comment  = search' "comment"
creationDate = search' "creation date"

{- Tracker accessors -}
tracker_complete, tracker_incomplete, tracker_interval :: BCode -> Maybe Integer
tracker_min_interval :: BCode -> Maybe Integer
tracker_complete = searchInt "complete"
tracker_incomplete = searchInt "incomplete"
tracker_interval = searchInt "interval"
tracker_min_interval = searchInt "min interval"

tracker_error, tracker_warning :: BCode -> Maybe String
tracker_error = searchStr "error"
tracker_warning = searchStr "warning"

tracker_peers :: BCode -> Maybe String
tracker_peers = searchStr "peers"

info :: BCode -> Maybe BCode
info = search [PString "info"]

infoLength, infoName, infoNameUtf8, infoPieceLength :: BCode -> Maybe BCode
infoLength = searchInfo "length"
infoName   = searchInfo "name"
infoNameUtf8 = searchInfo "name.utf-8"
infoPieceLength = searchInfo "piece length"

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
      BInt i -> int i
      BString s -> text s
      BArray arr -> cat $ intersperse comma al
          where al = map pp arr
      BDict mp -> cat $ intersperse comma mpl
          where mpl = map (\(s, bc') -> text s <+> text "->" <+> pp bc') $ M.toList mp

prettyPrint :: BCode -> String
prettyPrint = render . pp