-- | Add a module description here
--   also add descriptions to each function.
module Protocol.BCode 
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
              infoFiles,
              prettyPrint,

              trackerComplete,
              trackerIncomplete,
              trackerInterval,
              trackerMinInterval,
              trackerPeers,
              trackerWarning,
              trackerError,
              toBS,
              fromBS,
              -- Extended handshake
              extendedP,
              extendedV,
              extendedRReq,
              extendedMsg,
              --Tests
              testSuite)
where

import Control.Monad
import Control.Applicative hiding (many)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.Char

import Data.List
import qualified Data.Map as M
import Text.PrettyPrint.HughesPJ hiding (char)

import Data.Serialize
import Data.Word

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Path, Test)

import Digest
import TestInstance() -- for instances only

-- | BCode represents the structure of a bencoded file
data BCode = BInt Integer                       -- ^ An integer
           | BString B.ByteString               -- ^ A string of bytes
           | BArray [BCode]                     -- ^ An array
           | BDict (M.Map B.ByteString BCode)   -- ^ A key, value map
  deriving (Show, Eq)

instance Arbitrary BCode where
    arbitrary = sized bc'
      where bc' :: Int -> Gen BCode
            bc' 0 = oneof [BInt <$> arbitrary,
                           BString <$> arbitrary]
            bc' n =
                oneof [BInt <$> arbitrary,
                       BString <$> arbitrary,
                       BArray <$> sequence (replicate n $ bc' (n `div` 8)),
                       do keys <- vectorOf n arbitrary
                          values <- sequence (replicate n $ bc' (n `div` 8))
                          return $ BDict $ M.fromList $ zip keys values]

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
putShow = mapM_ put . show

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
hashInfoDict :: BCode -> IO Digest
hashInfoDict bc =
    do ih <- case info bc of
                Nothing -> fail "Could not find infoHash"
                Just x  -> return x
       let encoded = encode ih
       return $ digest $ L.fromChunks $ [encoded]


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

trackerPeers :: BCode -> Maybe (B.ByteString, B.ByteString)
trackerPeers bc = do v4 <- searchStr "peers" bc
                     v6 <- return $ maybe (B.empty) id $ searchStr "peers6" bc
                     return (v4, v6)

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
infoLength bc = maybe length2 Just length1
    where
      -- |info/length key for single-file torrent
      length1 = do BInt i <- search [toPS "info", toPS "length"] bc
                   return i
      -- |length summed from files of multi-file torrent
      length2 = sum `fmap`
                map snd `fmap`
                infoFiles bc

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

infoFiles :: BCode -> Maybe [([String], Integer)]  -- ^[(filePath, fileLength)]
infoFiles bc = let mbFpath = fromBS `fmap` infoName bc
                   mbLength = infoLength bc
                   mbFiles = do BArray fileList <- searchInfo "files" bc
                                return $ do fileDict@(BDict _) <- fileList
                                            let Just (BInt l) = search [toPS "length"] fileDict
                                                Just (BArray pth) = search [toPS "path"] fileDict
                                                pth' = map (\(BString s) -> fromBS s) pth
                                            return (pth', l)
               in case (mbFpath, mbLength, mbFiles) of
                    (Just fpath, _, Just files) ->
                        Just $
                        map (\(pth, l) ->
                                 (fpath:pth, l)
                            ) files
                    (Just fpath, Just l, _) ->
                        Just [([fpath], l)]
                    (_, _, Just files) ->
                        Just files
                    _ ->
                        Nothing

---------------------------------------------------------------------
-- Extended message handshake
--

extendedP :: BCode -> Maybe Word16
extendedP = fmap fromIntegral . searchInt "p"

extendedV :: BCode -> Maybe String
extendedV = fmap ( fmap (chr . fromIntegral) ) . fmap B.unpack . searchStr "v"

extendedRReq :: BCode -> Maybe Integer
extendedRReq = searchInt "rreq"

extendedMsg :: Integer -> String -> Integer -> BCode
extendedMsg p v rreq = BDict $ M.fromList [(toBS "m",    BDict M.empty)
                                          ,(toBS "p",    BInt p)
                                          ,(toBS "v",    BString $ toBS v)
                                          ,(toBS "rreq", BInt rreq)]

---------------------------------------------------------------------
-- Pretty printing
--

pp :: BCode -> Doc
pp bc =
    case bc of
      BInt i -> integer i
      BString s -> text (show s)
      BArray arr -> text "[" <+> (cat $ intersperse comma al) <+> text "]"
          where al = map pp arr
      BDict mp -> text "{" <+> cat (intersperse comma mpl) <+> text "}"
          where mpl = map (\(s, bc') -> text (fromBS s) <+> text "->" <+> pp bc') $ M.toList mp

prettyPrint :: BCode -> String
prettyPrint = render . pp


toBDict :: [(String,BCode)] -> BCode
toBDict = BDict . M.fromList . map (\(k,v) -> ((toBS k),v))

toBString :: String -> BCode
toBString = BString . toBS


-- TESTS


testSuite :: Test
testSuite = testGroup "Protocol/BCode"
  [ testProperty "QC encode-decode/id" propEncodeDecodeId,
    testCase "HUnit encode-decode/id" testDecodeEncodeProp1 ]

propEncodeDecodeId :: BCode -> Bool
propEncodeDecodeId bc =
    let encoded = encode bc
        decoded = decode encoded
    in
       Right bc == decoded

testDecodeEncodeProp1 :: Assertion
testDecodeEncodeProp1 =
    let encoded = encode testData
        decoded = decode encoded
    in
       assertEqual "for encode/decode identify" (Right testData) decoded

testData :: [BCode]
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

