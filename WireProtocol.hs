module WireProtocol
where

import Control.Applicative hiding (empty)
import Control.Monad

import Data.Monoid
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder
import Data.ByteString.Parser
import Data.Word
import System.IO

import Torrent

------------------------------------------------------------

type BitField = B.ByteString

type PieceOffset = Integer
type PieceLength = Integer
data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have PieceNum
             | BitField BitField
             | Request PieceNum PieceOffset PieceLength
             | Piece PieceNum PieceOffset B.ByteString
             | Cancel PieceNum PieceOffset PieceLength
             | Port Integer
  deriving (Eq, Show)


-- | The Protocol header for the Peer Wire Protocol
protocolHeader :: String
protocolHeader = "BitTorrent protocol"

extensionBasis :: Word64
extensionBasis = 0

extensionFast :: Word64
extensionFast = 4

putPieceInfo :: PieceOffset -> PieceLength -> Builder
putPieceInfo os sz = mconcat [pw os, pw sz]
  where pw = putWord32be . fromInteger

-- Seriously consider HCodecs as a dependency
encodeMsg :: Message -> Builder
encodeMsg KeepAlive       = empty
encodeMsg Choke           = singleton 0
encodeMsg Unchoke         = singleton 1
encodeMsg Interested      = singleton 2
encodeMsg NotInterested   = singleton 3
encodeMsg (Have pn)       = mconcat [singleton 4, putWord32be . fromInteger $ pn]
encodeMsg (BitField bf)   = mconcat [singleton 5, fromLazyByteString bf]
encodeMsg (Request pn os sz) = mconcat [singleton 6, putWord32be . fromInteger $ pn,
                                        putPieceInfo os sz]
encodeMsg (Piece pn os c) = mconcat [singleton 7, putWord32be . fromInteger $ pn,
                                     putWord32be . fromInteger $ os,
                                     fromLazyByteString c]
encodeMsg (Cancel pn os sz)  = mconcat [singleton 8, putWord32be . fromInteger $ pn,
                                        putPieceInfo os sz]
encodeMsg (Port p)        = mconcat [singleton 9, putWord16be . fromInteger $ p]

decodeMsg :: Parser Message
decodeMsg =
    do m <- getWord8
       case m of
         0 -> return Choke
         1 -> return Unchoke
         2 -> return Interested
         3 -> return NotInterested
         4 -> Have     <$> gw32
         5 -> BitField <$> getRemainingLazyByteString
         6 -> Request  <$> gw32 <*> gw32 <*> gw32
         7 -> Piece    <$> gw32 <*> gw32 <*> getRemainingLazyByteString
         8 -> Cancel   <$> gw32 <*> gw32 <*> gw32
         9 -> Port     <$> (fromIntegral <$> getWord16be)
         _ -> fail "Incorrect message parse"
  where gw32 = fromIntegral <$> getWord32be


-- | encode a message for transmit on a socket
encode :: Message -> B.ByteString
encode m = toLazyByteString $ mconcat [putWord32be . fromIntegral $ sz,
                                       bld]
  where bld = encodeMsg m
        sz = B.length $ toLazyByteString bld -- Suboptimal, but works :)


-- | Protocol handshake code. This encodes the protocol handshake part
protocolHandshake :: B.ByteString
protocolHandshake = toLazyByteString $ mconcat [putWord32be . fromIntegral $ sz,
                                                putString protocolHeader,
                                                putWord64be caps]
  where caps = extensionBasis
        sz = length protocolHeader

-- | Receive the header parts from the other end
receiveHeader :: Handle -> InfoHash -> IO (Either String ([Capabilities],
                                                          B.ByteString))
receiveHeader h ih =
    do handshake <- B.hGet h handshakeSize
       return $ parseHeader handshake
  where handshakeSize = 68
        protocolHeaderSize = length protocolHeader
        parseHeader = runParser parser
        parser =
            do hdSz <- getWord8
               when (fromIntegral hdSz /= protocolHeaderSize) $ fail "Wrong header size"
               protoString <- getString protocolHeaderSize
               when (protoString /= protocolHeader) $ fail "Wrong protocol header"
               caps <- getWord64be
               ihR   <- getLazyByteString 20
               when (ihR /= ih) $ fail "Wrong InfoHash"
               pid <- getLazyByteString 20
               return (decodeCapabilities caps, pid)

data Capabilities = Fast

decodeCapabilities :: Word64 -> [Capabilities]
decodeCapabilities _ = []

-- | Initiate a handshake on a socket
initiateHandshake :: Handle -> PeerId -> InfoHash -> IO (Either String ([Capabilities],
                                                                        B.ByteString))
initiateHandshake handle peerid infohash = do B.hPut handle msg
                                              receiveHeader handle infohash -- TODO: Exceptions
  where msg = toLazyByteString $ mconcat [fromLazyByteString protocolHandshake,
                                          fromLazyByteString infohash,
                                          putString peerid]

-- TESTS
testDecodeEncodeProp1 :: Message -> Bool
testDecodeEncodeProp1 m =
    let encoded = toLazyByteString $ encodeMsg m
        decoded = runParser decodeMsg encoded
    in case decoded of
         Left _ -> False
         Right m' -> m == m'

