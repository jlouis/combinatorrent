module WireProtocol
where

import qualified Data.ByteString.Lazy as B

import Torrent

type BitField = B.ByteString

data Message = KeepAlive
             | Choke
             | Unchoke
             | Interested
             | NotInterested
             | Have PieceNum
             | BitField BitField
             | Request PieceInfo
             | Piece PieceInfo B.ByteString
             | Cancel PieceInfo
             | Port Integer

-- Seriously consider HCodecs as a dependency
encodeMsg :: Message -> B.ByteString
encodeMsg KeepAlive      = B.empty
encodeMsg Choke          = B.singleton 0
encodeMsg Unchoke        = B.singleton 1
encodeMsg Interested     = B.singleton 2
encodeMsg NotInterested  = B.singleton 3
encodeMsg (Have _pn)     = B.singleton 4 -- More work needed from here
encodeMsg (BitField _bf) = B.singleton 5
encodeMsg (Request _pi)  = B.singleton 6
encodeMsg (Piece _pi _c) = B.singleton 7
encodeMsg (Cancel _pi)   = B.singleton 8
encodeMsg (Port _p)      = B.singleton 9

