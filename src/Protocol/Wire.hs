{-
    A parser and encoder for the BitTorrent wire protocol using the
    cereal package.
-}

{-# LANGUAGE EmptyDataDecls #-}

module Protocol.Wire
    ( Message(..)
    , msgSize
    , encodePacket
    , decodeMsg
    , getMsg -- ^ Run attoparsec-based parser on input
    , getAPMsg
    , BitField
    , constructBitField
    -- Handshaking
    , initiateHandshake
    , receiveHandshake
    -- Tests
    , testSuite
    )
where

import Control.Applicative hiding (empty)
import Control.Monad

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Data.Attoparsec as A
import Data.Bits (testBit, setBit)

import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get

import Data.Char
import Data.Maybe (catMaybes)
import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import Network.Socket.ByteString
import qualified Network.Socket.ByteString.Lazy as Lz

import System.Log.Logger

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Protocol.BCode as BCode (BCode, encode)
import Torrent

------------------------------------------------------------

type BitField    = B.ByteString

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
             | HaveAll
             | HaveNone
             | Suggest PieceNum
             | RejectRequest PieceNum Block
             | AllowedFast PieceNum
             | ExtendedMsg Word8 B.ByteString
  deriving (Eq, Show)

msgSize :: Message -> Int
msgSize KeepAlive           = 0
msgSize Choke               = 1
msgSize Unchoke             = 1
msgSize Interested          = 1
msgSize NotInterested       = 1
msgSize (Have _)            = 5
msgSize (BitField bf)       = B.length bf + 1
msgSize (Request _ _)       = 13
msgSize (Piece _ _ bs)      = 9 + B.length bs
msgSize (Cancel _ _)        = 13
msgSize (Port _)            = 3
msgSize HaveAll             = 1
msgSize HaveNone            = 1
msgSize (Suggest _)         = 5
msgSize (RejectRequest _ _) = 13
msgSize (AllowedFast _)     = 5
msgSize (ExtendedMsg _ bs)  = B.length bs + 2

instance Arbitrary Message where
    arbitrary = oneof [return KeepAlive, return Choke, return Unchoke, return Interested,
                       return NotInterested, return HaveAll, return HaveNone,
                       Suggest <$> pos,
                       RejectRequest <$> pos <*> arbitrary,
                       AllowedFast <$> pos,
                       Have <$> pos,
                       BitField <$> arbitrary,
                       Request <$> pos <*> arbitrary,
                       Piece <$> pos <*> pos <*> arbitrary,
                       ExtendedMsg <$> arbitrary <*> arbitrary,
                       Cancel <$> pos <*> arbitrary,
                       let bc :: Gen B.ByteString
                           bc = do b <- arbitrary :: Gen BCode.BCode
                                   return $ BCode.encode b
                       in ExtendedMsg 0 <$> bc,
                       Port <$> choose (0,16383)]
        where
            pos :: Gen Int
            pos = choose (0, 4294967296 - 1)


-- | The Protocol header for the Peer Wire Protocol
protocolHeader :: String
protocolHeader = "BitTorrent protocol"

p8 :: Word8 -> Put
p8 = putWord8

p32be :: Integral a => a -> Put
p32be = putWord32be . fromIntegral

decodeMsg :: Get Message
decodeMsg = {-# SCC "decodeMsg" #-} get

encodePacket :: Message -> L.ByteString
encodePacket m = L.fromChunks [szEnc, mEnc]
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
    put (BitField bf)   = p8 5 *> putByteString bf
    put (Request pn (Block os sz))
                        = p8 6 *> mapM_ p32be [pn,os,sz]
    put (Piece pn os c) = p8 7 *> mapM_ p32be [pn,os] *> putByteString c
    put (Cancel pn (Block os sz))
                        = p8 8 *> mapM_ p32be [pn,os,sz]
    put (Port p)        = p8 9 *> (putWord16be . fromIntegral $ p)
    put (Suggest pn)    = p8 0x0D *> p32be pn
    put (ExtendedMsg idx bs)
                        = p8 20 *> p8 (fromIntegral idx) *> putByteString bs
    put HaveAll         = p8 0x0E
    put HaveNone        = p8 0x0F
    put (RejectRequest pn (Block os sz))
                        = p8 0x10 *> mapM_ p32be [pn,os,sz]
    put (AllowedFast pn)
                        = p8 0x11 *> p32be pn

    get =  getKA      <|> getChoke
       <|> getUnchoke <|> getIntr
       <|> getNI      <|> getHave
       <|> getBF      <|> getReq
       <|> getPiece   <|> getCancel
       <|> getPort    <|> getHaveAll
       <|> getHaveNone
       <|> getSuggest <|> getRejectRequest
       <|> getAllowedFast
       <|> getExtendedMsg

getMsg :: Parser Message
getMsg = do
    l <- apW32be
    if l == 0
        then return KeepAlive
        else getAPMsg l

getAPMsg :: Int -> Parser Message
getAPMsg l = do
    c <- A.anyWord8
    case c of
        0  -> return Choke
        1  -> return Unchoke
        2  -> return Interested
        3  -> return NotInterested
        4  -> (Have <$> apW32be)
        5  -> (BitField <$> (A.take (l-1)))
        6  -> (Request <$> apW32be <*> (Block <$> apW32be <*> apW32be))
        7  -> (Piece <$> apW32be <*> apW32be <*> A.take (l - 9))
        8  -> (Cancel <$> apW32be <*> (Block <$> apW32be <*> apW32be))
        9  -> (Port . fromIntegral <$> apW16be)
        0x0D -> (Suggest <$> apW32be)
        0x0E -> return HaveAll
        0x0F -> return HaveNone
        0x10 -> (RejectRequest <$> apW32be <*> (Block <$> apW32be <*> apW32be))
        0x11 -> (AllowedFast <$> apW32be)
        20 -> (ExtendedMsg <$> A.anyWord8 <*> A.take (l - 2))
        k  -> fail $ "Illegal parse, code: " ++ show k

apW32be :: Parser Int
apW32be = do
    [b1,b2,b3,b4] <- replicateM 4 A.anyWord8
    let b1' = fromIntegral b1
        b2' = fromIntegral b2
        b3' = fromIntegral b3
        b4' = fromIntegral b4
    return (b4' + (256 * b3') + (256 * 256 * b2') + (256 * 256 * 256 * b1'))

apW16be :: Parser Int
apW16be = do
    [b1, b2] <- replicateM 2 A.anyWord8
    let b1' = fromIntegral b1
        b2' = fromIntegral b2
    return (b2' + 256 * b1')


getBF, getChoke, getUnchoke, getIntr, getNI, getHave, getReq :: Get Message
getPiece, getCancel, getPort, getKA :: Get Message
getRejectRequest, getAllowedFast, getSuggest, getHaveAll, getHaveNone :: Get Message
getExtendedMsg :: Get Message
getChoke         = byte 0  *> return Choke
getUnchoke       = byte 1  *> return Unchoke
getIntr          = byte 2  *> return Interested
getNI            = byte 3  *> return NotInterested
getHave          = byte 4  *> (Have <$> gw32)
getBF            = byte 5  *> (BitField <$> (remaining >>= getByteString . fromIntegral))
getReq           = byte 6  *> (Request  <$> gw32 <*> (Block <$> gw32 <*> gw32))
getPiece         = byte 7  *> (Piece    <$> gw32 <*> gw32 <*> (remaining >>= getByteString))
getCancel        = byte 8  *> (Cancel   <$> gw32 <*> (Block <$> gw32 <*> gw32))
getPort          = byte 9  *> (Port . fromIntegral <$> getWord16be)
getSuggest       = byte 0x0D *> (Suggest <$> gw32)
getHaveAll       = byte 0x0E *> return HaveAll
getHaveNone      = byte 0x0F *> return HaveNone
getRejectRequest = byte 0x10 *> (RejectRequest <$> gw32 <*> (Block <$> gw32 <*> gw32))
getAllowedFast   = byte 0x11 *> (AllowedFast <$> gw32)
getExtendedMsg   = byte 20 *> (ExtendedMsg <$> getWord8 <*> (remaining >>= getByteString))
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

toW8 :: Char -> Word8
toW8 = fromIntegral . ord


-- | Receive the header parts from the other end
receiveHeader :: Socket -> Int -> (InfoHash -> Bool)
              -> IO (Either String ([Capabilities], L.ByteString, InfoHash))
receiveHeader sock sz ihTst = parseHeader `fmap` loop [] sz
  where parseHeader = runGet (headerParser ihTst)
        loop :: [B.ByteString] -> Int -> IO B.ByteString
        loop bs z | z == 0 = return . B.concat $ reverse bs
                  | otherwise = do
                        nbs <- recv sock z
                        when (B.length nbs == 0) $ fail "Socket is dead"
                        loop (nbs : bs) (z - B.length nbs)


headerParser :: (InfoHash -> Bool) -> Get ([Capabilities], L.ByteString, InfoHash)
headerParser ihTst = do
    hdSz <- getWord8
    when (fromIntegral hdSz /= protocolHeaderSize) $ fail "Wrong header size"
    protoString <- getByteString protocolHeaderSize
    when (protoString /= toBS protocolHeader) $ fail "Wrong protocol header"
    caps <- getWord64be
    ihR  <- getByteString 20
    unless (ihTst ihR) $ fail "Unknown InfoHash"
    pid <- getLazyByteString 20
    return (decodeCapabilities caps, pid, ihR)

extensionBasis :: Word64
extensionBasis =
    (flip setBit 2) -- Fast extension
    . (flip setBit 20) -- Extended messaging support
    $ 0

decodeCapabilities :: Word64 -> [Capabilities]
decodeCapabilities w = catMaybes
    [ if testBit w 2 then Just Fast else Nothing,
      if testBit w 20 then Just Extended else Nothing ]

-- | Initiate a handshake on a socket
initiateHandshake :: Socket -> PeerId -> InfoHash
                  -> IO (Either String ([Capabilities], L.ByteString, InfoHash))
initiateHandshake sock peerid infohash = do
    debugM "Protocol.Wire" "Sending off handshake message"
    _ <- Lz.send sock msg
    debugM "Protocol.Wire" "Receiving handshake from other end"
    receiveHeader sock sz (== infohash)
  where msg = handShakeMessage peerid infohash
        sz = fromIntegral (L.length msg)

-- | Construct a default handshake message from a PeerId and an InfoHash
handShakeMessage :: PeerId -> InfoHash -> L.ByteString
handShakeMessage pid ih =
    L.fromChunks . map runPut $ [putLazyByteString protocolHandshake,
                                 putByteString ih,
                                 putByteString . toBS $ pid]

-- | Receive a handshake on a socket
receiveHandshake :: Socket -> PeerId -> (InfoHash -> Bool)
                 -> IO (Either String ([Capabilities], L.ByteString, InfoHash))
receiveHandshake s pid ihTst = do
    debugM "Protocol.Wire" "Receiving handshake from other end"
    r <- receiveHeader s sz ihTst
    case r of
        Left err -> return $ Left err
        Right (caps, rpid, ih) ->
            do debugM "Protocol.Wire" "Sending back handshake message"
               _ <- Lz.send s (msg ih)
               return $ Right (caps, rpid, ih)
  where msg ih = handShakeMessage pid ih
        sz = fromIntegral (L.length $ msg (B.pack $ replicate 20 32)) -- Dummy value


-- | The call @constructBitField pieces@ will return the a ByteString suitable for inclusion in a
--   BITFIELD message to a peer.
constructBitField :: Int -> [PieceNum] -> B.ByteString
constructBitField sz pieces = B.pack . build $ m
    where m = map (`elem` pieces) [0..sz-1 + pad]
          pad = case sz `mod` 8 of
                    0 -> 0
                    n -> 8 - n
          build [] = []
          build l = let (first, rest) = splitAt 8 l
                    in if length first /= 8
                       then error "Wront bitfield"
                       else bytify first : build rest
          bytify [b7,b6,b5,b4,b3,b2,b1,b0] = sum [if b0 then 1   else 0,
                                                  if b1 then 2   else 0,
                                                  if b2 then 4   else 0,
                                                  if b3 then 8   else 0,
                                                  if b4 then 16  else 0,
                                                  if b5 then 32  else 0,
                                                  if b6 then 64  else 0,
                                                  if b7 then 128 else 0]
          bytify _ = error "Bitfield construction failed"

--
-- -- TESTS
testSuite :: Test
testSuite = testGroup "Protocol/Wire"
  [ testProperty "QC encode-decode/id" propEncodeDecodeId
  , testProperty "QC encode-decode/id - attoparsec" propEncodeDecodeIdAP ]


propEncodeDecodeId :: Message -> Bool
propEncodeDecodeId m =
    let encoded = encode m
        decoded = decode encoded
    in
        Right m == decoded

propEncodeDecodeIdAP :: Message -> Bool
propEncodeDecodeIdAP m =
    let encoded = encodePacket m
        decoded = A.parse getMsg $ B.concat $ L.toChunks encoded
    in case decoded of
         A.Done r m2 -> B.null r && m == m2
         _           -> False

