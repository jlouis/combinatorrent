-- | The status code runs a Status Process. This process keeps track
--   of a number of interval valies for a given torrent file and it
--   periodically updates the tracker process with the relevant
--   information about data uploaded, downloaded and how much is
--   left. The tracker is then responsible for using this data
--   correctly to tell the tracker what to do
{-# LANGUAGE FlexibleInstances #-}
module Process.Status (
    -- * Types
      StatusMsg(..)
    , TrackerMsg(..)
    -- * Channels
    , StatusChan
    -- * State
    , StatusState(uploaded, downloaded, left)
    -- * Interface
    , start
    )
where

import Control.Concurrent
import Control.Concurrent.CML.Strict
import Control.Exception (assert)
import Control.DeepSeq

import Control.Monad.State

import qualified Data.Map as M

import Prelude hiding (log)
import Process
import Supervisor
import Torrent

data StatusMsg = TrackerStat { trackInfoHash :: InfoHash
                             , trackIncomplete :: Maybe Integer
                             , trackComplete   :: Maybe Integer }
               | CompletedPiece InfoHash Integer
               | InsertTorrent InfoHash Integer (Channel TrackerMsg)
               | RemoveTorrent InfoHash
               | PeerStat { peerInfoHash :: InfoHash
                          , peerUploaded :: Integer
                          , peerDownloaded :: Integer }
               | TorrentCompleted InfoHash
               | RequestStatus InfoHash (Channel StatusState)
               | RequestAllTorrents (Channel [(InfoHash, StatusState)])

instance NFData StatusMsg where
  rnf a = a `seq` ()

type StatusChan = Channel StatusMsg

-- | TrackerChannel is the channel of the tracker
data TrackerMsg = Stop | TrackerTick Integer | Start | Complete

instance NFData TrackerMsg where
   rnf a = a `seq` ()

data CF  = CF { statusCh :: Channel StatusMsg }

instance Logging CF where
    logName _ = "Process.Status"

type ST = M.Map InfoHash StatusState

data StatusState = SState
             { uploaded :: Integer
             , downloaded :: Integer
             , left :: Integer
             , incomplete :: Maybe Integer
             , complete :: Maybe Integer
             , state :: TorrentState
             , trackerMsgCh :: Channel TrackerMsg
             }

instance Show StatusState where
    show (SState up down left inc comp st _) = concat
        ["{ Uploaded:   " ++ show up ++ "\n"
        ,"  Downloaded: " ++ show down ++ "\n"
        ,"  Left:       " ++ show left ++ "\n"
        ,"  State:      " ++ show st ++ "\n"
        ,"  Complete:   " ++ show comp ++ "\n"
        ,"  Incomplete: " ++ show inc ++ " }"]

instance NFData StatusState where
  rnf a = a `seq` ()

instance NFData (Channel StatusState) where
  rnf a = a `seq` ()

-- | Start a new Status process with an initial torrent state and a
--   channel on which to transmit status updates to the tracker.
start :: Channel StatusMsg -> SupervisorChan -> IO ThreadId
start statusC supC = do
    spawnP (CF statusC) M.empty
        (catchP (foreverP pgm) (defaultStopHandler supC))
  where
    newMap left trackerMsgC =
        SState 0 0 left Nothing Nothing (if left == 0 then Seeding else Leeching) trackerMsgC
    pgm = {-# SCC "StatusP" #-} syncP =<< recvEvent
    recvEvent :: Process CF ST (Event ((), ST))
    recvEvent = do evt <- recvPC statusCh
                   wrapP evt (\m ->
                    case m of
                        TrackerStat ih ic c -> do
                            modify (\s -> M.adjust (\st -> st { incomplete = ic, complete = c }) ih s)
                        CompletedPiece ih bytes -> do
                            modify (\s -> M.adjust (\st -> st { left = (left st) - bytes }) ih s)
                        PeerStat ih up down -> do
                           modify (\s -> M.adjust (\st -> st { uploaded = (uploaded st) + up
                                                             , downloaded = (downloaded st) + down }) ih
                                                  s)
                        InsertTorrent ih left trackerMsgC ->
                            modify (\s -> M.insert ih (newMap left trackerMsgC) s)
                        RemoveTorrent ih -> modify (\s -> M.delete ih s)
                        RequestStatus ih retC -> do
                            s <- get
                            case M.lookup ih s of
                                Nothing -> fail "Unknown InfoHash"
                                Just st -> sendP retC st >>= syncP
                        RequestAllTorrents retC -> do
                            s <- get
                            sendP retC (M.toList s) >>= syncP
                        TorrentCompleted ih -> do
                            mp <- get
                            let q = M.lookup ih mp
                            ns  <- maybe (fail "Unknown Torrent") return q
                            assert (left ns /= 0) (return ())
                            syncP =<< sendP (trackerMsgCh ns) Complete
                            modify (\s -> M.insert ih (ns { state = Seeding}) s))
