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
import Control.Concurrent.CML.Strict
import Control.DeepSeq
import Control.Monad.State

import qualified Data.ByteString as B
import qualified Data.Map as M

import DeepSeqInstances()
import Process
import Torrent
import qualified FS
import Supervisor

data FSPMsg = CheckPiece PieceNum (Channel (Maybe Bool))
            | WriteBlock PieceNum Block B.ByteString
            | ReadBlock PieceNum Block (Channel B.ByteString)

instance NFData FSPMsg where
  rnf a = a `seq` ()

type FSPChannel = Channel FSPMsg

data CF = CF
      { fspCh :: FSPChannel -- ^ Channel on which to receive messages
      }

instance Logging CF where
  logName _ = "Process.FS"

data ST = ST
      { fileHandles :: FS.Handles -- ^ The file we are working on
      , pieceMap :: FS.PieceMap -- ^ Map of where the pieces reside
      }


-- INTERFACE
----------------------------------------------------------------------

start :: FS.Handles -> FS.PieceMap -> FSPChannel -> SupervisorChan-> IO ThreadId
start handles pm fspC supC =
    spawnP (CF fspC) (ST handles pm) (catchP (forever lp) (defaultStopHandler supC))
  where
    lp = {-# SCC "FS_Process" #-} msgEvent >>= syncP
    msgEvent = do
        ev <- recvPC fspCh
        wrapP ev (\msg ->
            case msg of
                CheckPiece n ch -> {-# SCC "FS_CheckPiece" #-} do
                    pm <- gets pieceMap
                    case M.lookup n pm of
                        Nothing -> sendP ch Nothing >>= syncP
                        Just pi -> do r <- gets fileHandles >>= (liftIO . FS.checkPiece pi)
                                      sendP ch (Just r) >>= syncP
                ReadBlock n blk ch -> {-# SCC "FS_ReadBlock" #-} do
                    debugP $ "Reading block #" ++ show n
                            ++ "(" ++ show (blockOffset blk) ++ ", " ++ show (blockSize blk) ++ ")"
                    -- TODO: Protection, either here or in the Peer code
                    h  <- gets fileHandles
                    bs <- gets pieceMap >>= (liftIO . FS.readBlock n blk h)
                    sendP ch bs >>= syncP
                WriteBlock pn blk bs -> {-# SCC "FS_WriteBlock" #-} do
                    -- TODO: Protection, either here or in the Peer code
                    fh <- gets fileHandles
                    pm <- gets pieceMap
                    liftIO $ FS.writeBlock fh pn blk pm bs)
