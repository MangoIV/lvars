{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
--
-- Parallel combinators based on top of a 'Par' monad.  Specifically, this module
-- provides higher order functions for operating on `Traversable` data structures in
-- parallel.
module Data.Par.Traversable
  ( -- * Naive parallel maps on traversable structures.

    -- | Because these operations assume only `Traversable`, the best they can do is
    -- to fork one parallel computation per element.
    parMap
  , ptraverse

    -- * Variants that evaluate only down to weak-head-normal-form
  , parMap_
  , ptraverse_
  )
where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad as M hiding (join, mapM, sequence)
import Control.Par.Class
import Control.Par.Class.Unsafe (internalLiftIO)
import Control.Par.EffectSigs
import Data.Traversable
import Prelude hiding (head, mapM, sequence, tail)

-- -----------------------------------------------------------------------------
-- Parallel maps over Traversable data structures
-- -----------------------------------------------------------------------------

-- | Applies the given function to each element of a data structure
-- in parallel (fully evaluating the results), and returns a new data
-- structure containing the results.
--
-- > parMap f xs = mapM (spawnP . f) xs >>= mapM get
--
-- @pmap@ is commonly used for lists, where it has this specialised type:
--
-- > parMap :: NFData b => (a -> b) -> [a] -> Par [b]
--
-- But note that for efficient parallelism you want balanced task trees, not "one at
-- a time" parallel tasks.  Thus look at `pmapReduce` and friends.
parMap
  :: (Traversable t, NFData b, ParFuture p, HasPut e, HasGet e, FutContents p b)
  => (a -> b)
  -> t a
  -> p e s (t b)
{-# INLINE parMap #-}
parMap f xs = mapM (spawnP . f) xs >>= mapM get

-- | A variant of `ParMap` that only evaluates to weak-head-normal-form.
parMap_
  :: (Traversable t, ParFuture p, HasGet e, HasPut e, FutContents p b)
  => (a -> b)
  -> t a
  -> p e s (t b)
{-# INLINE parMap_ #-}
parMap_ f xs = mapM spawnWHNF xs >>= mapM get
 where
  spawnWHNF x = spawn_ $ internalLiftIO $ evaluate (f x)

--------------------------------------------------------------------------------

-- | Like 'parMap', but the function is a @Par@ monad operation.
--
-- > ptraverse f xs = mapM (spawn . f) xs >>= mapM get
ptraverse
  :: (Traversable t, NFData b, ParFuture p, HasPut e, HasGet e, FutContents p b)
  => (a -> p e s b)
  -> t a
  -> p e s (t b)
{-# INLINE ptraverse #-}
ptraverse f xs = mapM (spawn . f) xs >>= mapM get

-- | A variant that only evaluates to weak-head-normal-form.
ptraverse_
  :: (Traversable t, ParFuture p, HasPut e, HasGet e, FutContents p b)
  => (a -> p e s b)
  -> t a
  -> p e s (t b)
{-# INLINE ptraverse_ #-}
ptraverse_ f xs = mapM (spawn_ . f) xs >>= mapM get

--------------------------------------------------------------------------------

-- TODO: parBuffer -- enable signaling with a counter?
