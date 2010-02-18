module WireProtocol
where

import Control.Applicative hiding (empty)
import Control.Monad

import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get

-- REMOVE LATER
-- import Data.ByteString.Builder
-- import Data.ByteString.Parser

import Data.Word
import Data.Char
import System.IO

import ConsoleP
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


-- | Protocol handshake code. This encodes the protocol handshake part
-- protocolHandshake :: L.ByteString
-- protocolHandshake = toLazyByteString $ mconcat [putWord8 $ fromIntegral sz,
--                                                 putString protocolHeader,
--                                                 putWord64be caps]
--   where caps = extensionBasis
--         sz = length protocolHeader

protocolHeaderSize = length protocolHeader


-- | Protocol handshake code. This encodes the protocol handshake part
protocolHandshake :: L.ByteString
protocolHandshake = L.fromChunks . map runPut $
                    [putWord8 $ fromIntegral protocolHeaderSize,
                     mapM_ (putWord8 . fromIntegral . ord) protocolHeader,
                     putWord64be extensionBasis]

toBS :: String -> B.ByteString
toBS = B.pack . map toW8

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
    ihR   <- getLazyByteString 20
    when (ihR /= ih) $ fail "Wrong InfoHash"
    pid <- getLazyByteString 20
    return (decodeCapabilities caps, pid)


data Capabilities = Fast
-- 
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
                                          putLazyByteString infohash,
                                          putByteString . toBS $ peerid]
        sz = fromIntegral (L.length msg)
-- 
-- -- TESTS
testDecodeEncodeProp1 :: Message -> Bool
testDecodeEncodeProp1 m =
    let encoded = encode m
        decoded = decode encoded
    in case decoded of
         Left _ -> False
         Right m' -> m == m'

-- Prelude.map testDecodeEncodeProp1 
testData = [KeepAlive,
            Choke,
            Unchoke,
            Interested,
            NotInterested,
            Have 0,
            Have 1,
            Have 1934729,
            BitField (L.pack [1,2,3]),
            Request 123 (Block 4 7),
            Piece 5 7 (B.pack [1,2,3,4,5,6,7,8,9,0]),
            Piece 5 7 (B.pack (concat . replicate 30 $ [minBound..maxBound])),
            Cancel 5 (Block 6 7),
            Port 123
           ]
-- Currently returns all True

