module WireProtocol
where

import Control.Applicative hiding (empty)
import Control.Monad

import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.ByteString.Parser
import Data.Word
import System.IO

import Logging
import Torrent

------------------------------------------------------------

type BitField = L.ByteString

type PieceLength = Int
data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have PieceNum
             | BitField BitField
             | Request PieceNum Block
             | Piece PieceNum Int B.ByteString
             | Cancel PieceNum Block
             | Port Integer
  deriving (Eq, Show)


-- | The Protocol header for the Peer Wire Protocol
protocolHeader :: String
protocolHeader = "BitTorrent protocol"

extensionBasis :: Word64
extensionBasis = 0

extensionFast :: Word64
extensionFast = 4


-- Seriously consider HCodecs as a dependency
encodeMsg :: Message -> Builder
encodeMsg KeepAlive       = empty
encodeMsg Choke           = singleton 0
encodeMsg Unchoke         = singleton 1
encodeMsg Interested      = singleton 2
encodeMsg NotInterested   = singleton 3
encodeMsg (Have pn)       =
    mconcat [singleton 4,
             putW32be pn
            ]
encodeMsg (BitField bf)   =
    mconcat [singleton 5,
             fromLazyByteString bf
            ]
encodeMsg (Request pn (Block os sz)) =
    mconcat [singleton 6,
             putW32be pn,
             putW32be os,
             putW32be sz
            ]
encodeMsg (Piece pn os c) =
    mconcat [singleton 7,
             putW32be pn,
             putW32be os,
             fromByteString c
            ]
encodeMsg (Cancel pn (Block os sz))  =
    mconcat [singleton 8,
             putW32be pn,
             putW32be os,
             putW32be sz
            ]
encodeMsg (Port p)        =
    mconcat [singleton 9,
             putW32be p
            ]

-- Helper function to make code above clearer
putW32be :: Integral a => a -> Builder
putW32be = putWord32be . fromIntegral

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
         6 -> Request  <$> gw32 <*> (Block <$> gw32 <*> gw32)
         7 -> Piece    <$> gw32 <*> gw32 <*> (remaining >>= getByteString . fromIntegral) --getRemainingLazyByteString)
         8 -> Cancel   <$> gw32 <*> (Block <$> gw32 <*> gw32)
         9 -> Port . fromIntegral <$> getWord16be
         _ -> fail "Incorrect message parse"
  where gw32 = fromIntegral <$> getWord32be

-- | encode a message for transmit on a socket
encode :: Message -> L.ByteString
encode m = toLazyByteString $ mconcat [putW32be sz, bld]
  where bld = encodeMsg m
        sz = L.length $ toLazyByteString bld -- Suboptimal, but works :)


-- | Protocol handshake code. This encodes the protocol handshake part
protocolHandshake :: L.ByteString
protocolHandshake = toLazyByteString $ mconcat [putWord8 $ fromIntegral sz,
                                                putString protocolHeader,
                                                putWord64be caps]
  where caps = extensionBasis
        sz = length protocolHeader

-- | Receive the header parts from the other end
receiveHeader :: Handle
              -> Int
              -> InfoHash
              -> IO (Either String ([Capabilities], L.ByteString))
receiveHeader h sz ih = parseHeader `fmap` L.hGet h sz
    -- do handshake <- L.hGet h sz
    --    return $ parseHeader handshake
  where protocolHeaderSize = length protocolHeader
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
initiateHandshake :: LogChannel ->
                     Handle -> PeerId -> InfoHash -> IO (Either String ([Capabilities],
                                                                        L.ByteString))
initiateHandshake logC handle peerid infohash = do
    logMsg logC "Sending off handshake message"
    L.hPut handle msg
    hFlush handle
    logMsg logC "Receiving handshake from other end"
    receiveHeader handle sz infohash -- TODO: Exceptions
  where msg = toLazyByteString $ mconcat [fromLazyByteString protocolHandshake,
                                          fromLazyByteString infohash,
                                          putString peerid]
        sz = fromIntegral (L.length msg)

-- TESTS
testDecodeEncodeProp1 :: Message -> Bool
testDecodeEncodeProp1 m =
    let encoded = toLazyByteString $ encodeMsg m
        decoded = runParser decodeMsg encoded
    in case decoded of
         Left _ -> False
         Right m' -> m == m'

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
