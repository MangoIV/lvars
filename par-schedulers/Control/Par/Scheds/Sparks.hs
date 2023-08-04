{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Par.Scheds.Sparks
  ( Par
  , runPar
  , runParPoly
  )
where

import Control.Monad (void)
import Control.Par.Class
import qualified Control.Par.Class.Unsafe as PC
import Control.Par.EffectSigs
import System.IO.Unsafe (unsafePerformIO)

newtype Par (e :: EffectSig) s a = Sparks {unwrapSparks :: S.Par a}

newtype SparksFuture s a = SparksFuture {unwrapSparksFuture :: S.Future a}
