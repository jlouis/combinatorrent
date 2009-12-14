module WireProtocol
where

import Control.Applicative hiding (empty)
import Control.Monad

import Data.Monoid
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder
import Data.ByteString.Parser

import Torrent

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
             | Piece PieceNum PieceOffset PieceLength B.ByteString
             | Cancel PieceNum PieceOffset PieceLength
             | Port Integer

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
encodeMsg (Piece pn os sz c) = mconcat [singleton 7, putWord32be . fromInteger $ pn,
                                        putPieceInfo os sz,
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
         7 -> Piece    <$> gw32 <*> gw32 <*> gw32 <*> getRemainingLazyByteString
         8 -> Cancel   <$> gw32 <*> gw32 <*> gw32
         9 -> Port     <$> (fromIntegral <$> getWord16be)
         _ -> fail "Incorrect message parse"
  where gw32 = fromIntegral <$> getWord32be


-- We should consider Message encoding directly on the socket rather than this thing
encode :: Message -> B.ByteString
encode m = toLazyByteString $ mconcat [putWord32be . fromIntegral $ sz,
                                       bld]
  where bld = encodeMsg m
        sz = B.length $ toLazyByteString bld -- Suboptimal, but works :)

