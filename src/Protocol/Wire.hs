{-
    A parser and encoder for the BitTorrent wire protocol using the
    cereal package.
-}


module Protocol.Wire
    ( Message(..)
    , encodePacket
    , decodeMsg
    , constructBitField
    , initiateHandshake
    -- Tests
    , testSuite
    )
where

import Control.Applicative hiding (empty)
import Control.Monad

import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get

import Data.Char
import System.IO

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Logging
import Torrent

------------------------------------------------------------

type BitField    = L.ByteString
type PieceLength = Int

data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have PieceNum -- Int
             | BitField BitField
             | Request PieceNum Block
             | Piece PieceNum Int B.ByteString
             | Cancel PieceNum Block
             | Port Integer
  deriving (Eq, Show)

instance Arbitrary Message where
    arbitrary = oneof [return KeepAlive, return Choke, return Unchoke, return Interested,
                       return NotInterested,
                       Have <$> arbitrary,
                       BitField <$> arbitrary,
                       Request <$> arbitrary <*> arbitrary,
                       Piece <$> arbitrary <*> arbitrary <*> arbitrary,
                       Cancel <$> arbitrary <*> arbitrary,
                       Port <$> choose (0,16383)]


data HandShake = HandShake 
                  String -- Protocol Header
                  Word64 -- Extension Bias

-- | The Protocol header for the Peer Wire Protocol
protocolHeader :: String
protocolHeader = "BitTorrent protocol"

extensionBasis :: Word64
extensionBasis = 0

extensionFast :: Word64
extensionFast = 4

p8 :: Word8 -> Put
p8 = putWord8

p32be :: Integral a => a -> Put
p32be = putWord32be . fromIntegral

decodeMsg :: Get Message
decodeMsg = get

encodePacket :: Message -> B.ByteString
encodePacket m = mconcat [szEnc, mEnc]
  where mEnc  = encode m
        sz    = B.length mEnc
        szEnc = runPut . p32be $ sz

instance Serialize Message where
    put KeepAlive       = return ()
    put Choke           = p8 0
    put Unchoke         = p8 1
    put Interested      = p8 2
    put NotInterested   = p8 3
    put (Have pn)       = p8 4 *> p32be pn
    put (BitField bf)   = p8 5 *> putLazyByteString bf
    put (Request pn (Block os sz))
                        = p8 6 *> mapM_ p32be [pn,os,sz]
    put (Piece pn os c) = p8 7 *> mapM_ p32be [pn,os] *> putByteString c
    put (Cancel pn (Block os sz))
                        = p8 8 *> mapM_ p32be [pn,os,sz]
    put (Port p)        = p8 9 *> (putWord16be . fromIntegral $ p)

    get =  getKA      <|> getChoke
       <|> getUnchoke <|> getIntr
       <|> getNI      <|> getHave
       <|> getBF      <|> getReq
       <|> getPiece   <|> getCancel
       <|> getPort

getChoke   = byte 0 *> return Choke
getUnchoke = byte 1 *> return Unchoke
getIntr    = byte 2 *> return Interested
getNI      = byte 3 *> return NotInterested
getHave    = byte 4 *> (Have <$> gw32)
getBF      = byte 5 *> (BitField <$> (remaining >>= getLazyByteString . fromIntegral))
getReq     = byte 6 *> (Request  <$> gw32 <*> (Block <$> gw32 <*> gw32))
getPiece   = byte 7 *> (Piece    <$> gw32 <*> gw32 <*> (remaining >>= getByteString))
getCancel  = byte 8 *> (Cancel   <$> gw32 <*> (Block <$> gw32 <*> gw32))
getPort    = byte 9 *> (Port . fromIntegral <$> getWord16be)
getKA      = do
    empty <- isEmpty
    if empty
        then return KeepAlive
        else fail "Non empty message - not a KeepAlive"

gw32 :: Integral a => Get a
gw32 = fromIntegral <$> getWord32be

byte :: Word8 -> Get Word8
byte w = do
    x <- lookAhead getWord8
    if x == w
        then getWord8
        else fail $ "Expected byte: " ++ show w ++ " got: " ++ show x

-- | Size of the protocol header
protocolHeaderSize :: Int
protocolHeaderSize = length protocolHeader

-- | Protocol handshake code. This encodes the protocol handshake part
protocolHandshake :: L.ByteString
protocolHandshake = L.fromChunks . map runPut $
                    [putWord8 $ fromIntegral protocolHeaderSize,
                     mapM_ (putWord8 . fromIntegral . ord) protocolHeader,
                     putWord64be extensionBasis]

toBS :: String -> B.ByteString
toBS = B.pack . map toW8

toLBS :: String -> L.ByteString
toLBS = L.pack . map toW8

toW8 :: Char -> Word8
toW8 = fromIntegral . ord

-- | Receive the header parts from the other end
receiveHeader :: Handle -> Int -> InfoHash
              -> IO (Either String ([Capabilities], L.ByteString))
receiveHeader h sz ih = parseHeader `fmap` B.hGet h sz
  where parseHeader = runGet (headerParser ih)

headerParser :: InfoHash -> Get ([Capabilities], L.ByteString)
headerParser ih = do
    hdSz <- getWord8
    when (fromIntegral hdSz /= protocolHeaderSize) $ fail "Wrong header size"
    protoString <- getByteString protocolHeaderSize
    when (protoString /= toBS protocolHeader) $ fail "Wrong protocol header"
    caps <- getWord64be
    ihR  <- getLazyByteString 20
    when (ihR /= toLBS ih) $ fail "Wrong InfoHash"
    pid <- getLazyByteString 20
    return (decodeCapabilities caps, pid)


data Capabilities = Fast
decodeCapabilities :: Word64 -> [Capabilities]
decodeCapabilities _ = []

-- | Initiate a handshake on a socket
initiateHandshake :: LogChannel -> Handle -> PeerId -> InfoHash
                  -> IO (Either String ([Capabilities], L.ByteString))
initiateHandshake logC handle peerid infohash = do
    logMsg logC "Sending off handshake message"
    L.hPut handle msg
    hFlush handle
    logMsg logC "Receiving handshake from other end"
    receiveHeader handle sz infohash -- TODO: Exceptions
  where msg = L.fromChunks . map runPut $ [putLazyByteString protocolHandshake,
                                           putLazyByteString $ toLBS infohash,
                                           putByteString . toBS $ peerid]
        sz = fromIntegral (L.length msg)

-- | The call @constructBitField pieces@ will return the a ByteString suitable for inclusion in a
--   BITFIELD message to a peer.
constructBitField :: Int -> [PieceNum] -> L.ByteString
constructBitField sz pieces = L.pack . build $ m
    where m = map (`elem` pieces) [0..sz-1 + pad]
          pad = 8 - (sz `mod` 8)
          build [] = []
          build l = let (first, rest) = splitAt 8 l
                    in if length first /= 8
                       then error "Wront bitfield"
                       else bytify first : build rest
          bytify [b7,b6,b5,b4,b3,b2,b1,b0] = sum [if b0 then 1 else 0,
                                                  if b1 then 2 else 0,
                                                  if b2 then 4 else 0,
                                                  if b3 then 8 else 0,
                                                  if b4 then 16 else 0,
                                                  if b5 then 32 else 0,
                                                  if b6 then 64 else 0,
                                                  if b7 then 128 else 0]

--
-- -- TESTS

testSuite = testGroup "Protocol/Wire"
  [ testProperty "QC encode-decode/id" propEncodeDecodeId]


propEncodeDecodeId :: Message -> Bool
propEncodeDecodeId m =
    let encoded = encode m
        decoded = decode encoded
    in
        Right m == decoded


