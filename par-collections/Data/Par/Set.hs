{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Provide instances for parallel handling of common, pure Haskell data structures.

module Data.Par.Set
       () where

import qualified Control.Par.Class     as PC
import qualified Data.Foldable         as F
import qualified Data.Set              as S
import           Data.Splittable.Class (Split (..))

--------------------------------------------------------------------------------

instance PC.Generator (S.Set a) where
  type ElemOf (S.Set a) = a
  {-# INLINE foldM #-}
  foldM = F.foldlM
  {-# INLINE fold #-}
  fold  = F.foldl'


#ifdef NEWCONTAINERS

instance Eq a => Split (S.Set a) where
  {-# INLINE split #-}
  split = S.splitRoot

-- TODO: Opt in to the trivial instance of ParFoldable, using Split-based mapreduce:
-- instance PC.ParFoldable (M.Map k v) where
--  pmapFold = Sp.pmapReduce
#else
-- instance PC.ParFoldable (M.Map k v) where
#endif
