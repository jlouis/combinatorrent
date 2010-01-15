-- | Core Process code
{-# LANGUAGE ExistentialQuantification, FlexibleInstances,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, TypeSynonymInstances, CPP #-}
-- required for deriving Typeable
{-# OPTIONS_GHC -fglasgow-exts #-}
module Process

where

import Control.Concurrent.CML

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Logging

import Data.Typeable

newtype Process a b c = Process (ReaderT a (StateT b IO) c)
#ifndef __HADDOCK__
  deriving (Functor, Monad, MonadIO, MonadState b, MonadReader a, Typeable)
#endif

class Logging a where
  getLogger :: a -> LogChannel

log :: Logging a => Process a b c -> String -> Process a b ()
log pcs msg = do
    logC <- asks getLogger
    liftIO $ logMsg logC msg


