
-- | For debugging purposes, it can be useful to lift an IO computation into an LVish @Par@ monad.
--
--   This module is imported for instances only (specifically, the `MonadIO` instance).

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.LVish.Internal.Unsafe() where

import           Control.LVish.Internal
import qualified Control.LVish.Internal.SchedIdempotent as L
import           Control.Monad.IO.Class

instance MonadIO (Par e s) where
  liftIO = WrapPar . L.liftIO
