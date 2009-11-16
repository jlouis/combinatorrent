module BCode (
              BCode,
              Path(..),
              encode,
              decode,
              search
             )

where

import Data.Maybe
import qualified Data.Map as M

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

-- Use parsec for this bastard
decode :: String -> BCode
decode = BString


{- Simple search function over BCoded data structures, general case. In practice, we
   will prefer some simpler mnemonics -}
search :: [Path] -> BCode -> BCode
search [] bc = bc
search (PInt i : rest) (BArray bs) = search rest (bs!!i)
search (PString s : rest) (BDict mp) = search rest (fromJust $ M.lookup s mp)
