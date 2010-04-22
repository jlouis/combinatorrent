-- | File system process. Acts as a maintainer for the filesystem in
--   question and can only do single-file torrents. It should be
--   fairly easy to add Multi-file torrents by altering this file and
--   the FS module.
{-# LANGUAGE ScopedTypeVariables #-}
module Process.FS
    ( FSPChannel
    , FSPMsg(..)
    , start
    )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State

import Data.Array
import qualified Data.ByteString as B

import Process
import Torrent
import qualified FS
import Supervisor

data FSPMsg = CheckPiece PieceNum (TMVar (Maybe Bool))
            | WriteBlock PieceNum Block B.ByteString
            | ReadBlock PieceNum Block (TMVar B.ByteString)

type FSPChannel = TChan FSPMsg

data CF = CF
      { fspCh :: FSPChannel -- ^ Channel on which to receive messages
      }

instance Logging CF where
  logName _ = "Process.FS"

data ST = ST
      { fileHandles :: !FS.Handles  -- ^ The file we are working on
      , pieceMap ::    !FS.PieceMap -- ^ Map of where the pieces reside
      }


-- INTERFACE
----------------------------------------------------------------------

start :: FS.Handles -> FS.PieceMap -> FSPChannel -> SupervisorChannel -> IO ThreadId
start handles pm fspC supC =
    spawnP (CF fspC) (ST handles pm) ({-# SCC "FS" #-}
                                        catchP lp (defaultStopHandler supC))
  where
    lp = do
        c <- asks fspCh
        msg <- liftIO . atomically $ readTChan c
        case msg of
           CheckPiece n v -> do
               pmap <- gets pieceMap
               let p = pmap ! n
               r <- gets fileHandles >>= (liftIO . FS.checkPiece p)
               liftIO . atomically $ putTMVar v (Just r)
           ReadBlock n blk v -> do
               debugP $ "Reading block #" ++ show n
                       ++ "(" ++ show (blockOffset blk) ++ ", " ++ show (blockSize blk) ++ ")"
               -- TODO: Protection, either here or in the Peer code
               h  <- gets fileHandles
               bs <- gets pieceMap >>= (liftIO . FS.readBlock n blk h)
               liftIO . atomically $ putTMVar v bs
           WriteBlock pn blk bs -> {-# SCC "FS_WriteBlock" #-} do
               -- TODO: Protection, either here or in the Peer code
               fh <- gets fileHandles
               pmap <- gets pieceMap
               liftIO $ FS.writeBlock fh pn blk pmap bs
        lp
