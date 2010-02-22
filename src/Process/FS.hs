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
import Control.Concurrent.CML
import Control.Monad.State

import qualified Data.ByteString as B
import qualified Data.Map as M

import Logging
import Process
import Torrent
import qualified FS
import Supervisor

data FSPMsg = CheckPiece PieceNum (Channel (Maybe Bool))
            | WriteBlock PieceNum Block B.ByteString
            | ReadBlock PieceNum Block (Channel B.ByteString)

type FSPChannel = Channel FSPMsg

data CF = CF
      { fspCh :: FSPChannel -- ^ Channel on which to receive messages
      , logCh :: LogChannel -- ^ Channel for logging
      }

instance Logging CF where
  getLogger cf = ("FSP", logCh cf)

data ST = ST
      { fileHandles :: FS.Handles -- ^ The file we are working on
      , pieceMap :: FS.PieceMap -- ^ Map of where the pieces reside
      }


-- INTERFACE
----------------------------------------------------------------------

start :: FS.Handles -> LogChannel -> FS.PieceMap -> FSPChannel -> SupervisorChan-> IO ThreadId
start handles logC pm fspC supC =
    spawnP (CF fspC logC) (ST handles pm) (catchP (forever lp) (defaultStopHandler supC))
  where
    lp = msgEvent >>= syncP
    msgEvent = do
        ev <- recvPC fspCh
        wrapP ev (\msg ->
            case msg of
                CheckPiece n ch -> do
                    pm <- gets pieceMap
                    case M.lookup n pm of
                        Nothing -> sendP ch Nothing >>= syncP
                        Just pi -> do r <- gets fileHandles >>= (liftIO . FS.checkPiece pi)
                                      sendP ch (Just r) >>= syncP
                ReadBlock n blk ch -> do
                    logDebug $ "Reading block #" ++ show n
                            ++ "(" ++ show (blockOffset blk) ++ ", " ++ show (blockSize blk) ++ ")"
                    -- TODO: Protection, either here or in the Peer code
                    h  <- gets fileHandles
                    bs <- gets pieceMap >>= (liftIO . FS.readBlock n blk h)
                    sendP ch bs >>= syncP
                WriteBlock pn blk bs -> do
                    -- TODO: Protection, either here or in the Peer code
                    fh <- gets fileHandles
                    pm <- gets pieceMap
                    liftIO $ FS.writeBlock fh pn blk pm bs)

checkPiece :: FSPChannel -> PieceNum -> IO (Maybe Bool)
checkPiece fspC n = do
    ch <- channel
    sync . transmit fspC $ CheckPiece n ch
    sync $ receive ch (const True)

