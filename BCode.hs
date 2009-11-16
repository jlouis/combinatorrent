module BCode (
              BCode,
              Path(..),
              encode,
              decode,
              search,
              prettyPrint
             )

where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Text.PrettyPrint.HughesPJ hiding (char)

data BCode = BInt Int
           | BString String
           | BArray [BCode]
           | BDict (M.Map String BCode)

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
search :: [Path] -> BCode -> BCode
search [] bc = bc
search (PInt i : rest) (BArray bs) = search rest (bs!!i)
search (PString s : rest) (BDict mp) = search rest (fromJust $ M.lookup s mp)

pp :: BCode -> Doc
pp bc =
    case bc of
      BInt i -> int i
      BString s -> text s
      BArray arr -> cat $ intersperse comma al
          where al = map pp arr
      BDict mp -> cat $ intersperse comma mpl
          where mpl = map (\(s, bc) -> text s <+> text "->" <+> pp bc) $ M.toList mp

prettyPrint :: BCode -> String
prettyPrint = render . pp