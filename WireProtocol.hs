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

data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have PieceNum
             | BitField BitField
             | Request PieceNum PieceInfo
             | Piece PieceNum PieceInfo B.ByteString
             | Cancel PieceNum PieceInfo
             | Port Integer

putPieceInfo :: PieceInfo -> Builder
putPieceInfo p = mconcat [pw . offset $ p,
                           pw . len    $ p]
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
encodeMsg (Request pn p) = mconcat [singleton 6, putWord32be . fromInteger $ pn,
                                    putPieceInfo p]
encodeMsg (Piece pn p c) = mconcat [singleton 7, putWord32be . fromInteger $ pn,
                                     putPieceInfo p,
                                     fromLazyByteString c]
encodeMsg (Cancel pn p)  = mconcat [singleton 8, putWord32be . fromInteger $ pn,
                                     putPieceInfo p]
encodeMsg (Port p)        = mconcat [singleton 9, putWord16be . fromInteger $ p]

decodeMsg :: Parser Message
decodeMsg =
    do m <- getWord8
       case m of
         0 -> return Choke
         1 -> return Unchoke
         2 -> return Interested
         3 -> return NotInterested
         4 -> do pn <- fromIntegral <$> getWord32be
                 return $ Have pn
         5 -> do bf <- getRemainingLazyByteString
                 return $ BitField bf
         6 -> do pn <- fromIntegral <$> getWord32be
                 p <- getPieceInfo
                 return $ Request pn p
         7 -> do pn <- fromIntegral <$> getWord32be
                 p <- getPieceInfo
                 c <- getRemainingLazyByteString
                 return $ Piece pn p c
         8 -> do pn <- fromIntegral <$> getWord32be
                 p <- getPieceInfo
                 return $ Cancel pn p
         9 -> do prt <- fromIntegral <$> getWord16be
                 return $ Port prt
         _ -> fail "Incorrect message parse"
  where getPieceInfo = do os <- fromIntegral <$> getWord32be
                          sz <- fromIntegral <$> getWord32be
                          return $ PieceInfo os sz "" -- Leave the digest empty


-- We should consider Message encoding directly on the socket rather than this thing
encode :: Message -> B.ByteString
encode m = toLazyByteString $ mconcat [putWord32be . fromIntegral $ sz,
                                       bld]
  where bld = encodeMsg m
        sz = B.length $ toLazyByteString bld -- Suboptimal, but works :)

