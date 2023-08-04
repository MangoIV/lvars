{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: a more detailed (recursive?) DeepFrz instance example might
-- be really helpful here for people who want to implement their own
-- LVar types. -- LK

module Control.LVish.DeepFrz
  ( -- * The functions you'll want to use
    runParThenFreeze
  , runParThenFreezeIO

    -- * Some supporting types
  , DeepFrz ()
  , FrzType
  , NonFrzn
  , Frzn
  , Trvrsbl
  )
where

-- import Control.LVish (LVarData1(..))
import Control.LVish.DeepFrz.Internal
  ( DeepFrz (..)
  , Frzn
  , NonFrzn
  , Trvrsbl
  )
import Control.LVish.Internal (Par (WrapPar))
import Control.LVish.Internal.SchedIdempotent (runPar, runParIO)
import Control.Par.EffectSigs

--------------------------------------------------------------------------------

-- | Under normal conditions, calling a `freeze` operation inside a
-- `Par` computation makes the `Par` computation quasi-deterministic.
-- However, if we freeze only after all LVar operations are completed
-- (after the implicit global barrier of `runPar`), then we've avoided
-- all data races, and freezing is therefore safe.  Running a `Par`
-- computation with `runParThenFreeze` accomplishes this, without our
-- having to call `freeze` explicitly.
--
-- In order to use `runParThenFreeze`, the type returned from the
-- `Par` computation must be a member of the `DeepFrz` class.  All the
-- @Data.LVar.*@ libraries should provide instances of `DeepFrz`
-- already.  Further, you can create additional instances for custom,
-- pure datatypes.  The result of a `runParThenFreeze` depends on the
-- type-level function `FrzType`, whose only purpose is to toggle the
-- `s` parameters of all IVars to the `Frzn` state.
--
-- Significantly, the freeze at the end of `runParThenFreeze` has /no/ runtime cost, in
-- spite of the fact that it enables a /deep/ (recursive) freeze of the value returned
-- by the `Par` computation.
runParThenFreeze :: (DeepFrz a, Deterministic e) => Par e NonFrzn a -> FrzType a
-- runParThenFreeze :: Deterministic e => DeepFrz a => Par e NonFrzn a -> FrzType a
runParThenFreeze (WrapPar p) = frz $ runPar p

-- | This version works for nondeterministic computations as well.
--
-- Of course, nondeterministic computations may also call `freeze`
-- internally, but this function has an advantage to doing your own
-- `freeze` at the end of a `runParIO`: there is an implicit barrier
-- before the final freeze.  Further, `DeepFrz` has no runtime
-- overhead, whereas regular freezing has a cost.
runParThenFreezeIO :: (DeepFrz a) => Par e NonFrzn a -> IO (FrzType a)
runParThenFreezeIO (WrapPar p) = do
  x <- runParIO p
  return $ frz x

{-
-- This won't work because it conflicts with other instances such as "Either":
instance (LVarData1 f, DeepFrz a) => DeepFrz (f s a) where
  type FrzType (f s a) = f Frzn (FrzType a)
  frz = unsafeCoerce#
-}
