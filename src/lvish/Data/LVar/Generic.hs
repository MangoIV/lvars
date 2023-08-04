{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE Trustworthy     #-}
{-# LANGUAGE TypeFamilies    #-}

-- | A generic interface providing operations that work on /all/ LVars.

module Data.LVar.Generic
       (
         -- * Classes containing the generic interfaces
         LVarData1(..), LVarWBottom(..),
         OrderedLVarData1(..),

         -- * Supporting types and utilities
         AFoldable(..),
         castFrzn, forFrzn,
         PartialJoinSemiLattice(..)
       )
       where

-- import           Control.LVish.Internal.Types
import           Control.LVish.Internal.Basics
import           Control.Par.EffectSigs
-- -- import           Control.LVish.Internal (Par)
import           Control.LVish.DeepFrz.Internal (Frzn, Trvrsbl)
import qualified Data.Foldable                  as F
-- -- import           Data.List (sort)
-- -- import           GHC.Prim (unsafeCoerce#)
import           Data.LVar.Generic.Internal
import           System.IO.Unsafe               (unsafeDupablePerformIO)

--------------------------------------------------------------------------------

-- | Some LVar datatypes are stored in an /internally/ ordered way so
-- that it is then possible to take /O(1)/ frozen snapshots and consume them
-- inexpensively in a deterministic order.
--
-- LVars with this additional property provide this class as well as `LVarData1`.
class LVarData1 f => OrderedLVarData1 (f :: * -> * -> *) where
  -- | Don't just freeze the LVar, but make the full contents
  -- completely available and `Foldable`.  Guaranteed /O(1)/.
  snapFreeze :: HasFreeze e => f s a -> Par e s (f Trvrsbl a)

{-
-- | Just like LVarData1 but for type constructors of kind `*`.
class LVarData0 (t :: *) where
  -- | This associated type models a picture of the "complete" contents of the data:
  -- e.g. a whole set instead of one element, or the full/empty information for an
  -- IVar, instead of just the payload.
  type Snapshot0 t
  freeze0 :: HasFreeze e => t -> Par e s (Snapshot0 t)
  newBottom0 :: Par e s t
-}

-- | A partial version of "Algebra.Lattice.JoinSemiLattice", this
-- could be made into a complete lattice by the addition of a top
-- element.
class PartialJoinSemiLattice a where
  joinMaybe :: a -> a -> Maybe a

instance PartialJoinSemiLattice Int where
  joinMaybe a b
    | even a && even b = Just (max a b)
    | odd  a && odd  b = Just (max a b)
    | otherwise        = Nothing

------------------------------------------------------------------------------
-- Dealing with frozen LVars.
------------------------------------------------------------------------------

-- | `Trvrsbl` is a stronger property than `Frzn`, so it is always safe to \"upcast\" to
-- the weaker version.
castFrzn :: LVarData1 f => f Trvrsbl a -> f Frzn a
castFrzn x = unsafeCoerceLVar x

-- | LVish `Par` actions must commute, therefore one safe way to consume a frozen (but
-- unordered) LVar, even in another `runPar` session, is to run a `Par` computation for
-- each element.
forFrzn :: LVarData1 f => f Frzn a -> (a -> Par e s ()) -> Par e s ()
forFrzn fzn fn =
  F.foldrM (\ a () -> fn a) () $
    unsafeDupablePerformIO $ -- ASSUME idempotence.
    unsafeTraversable fzn


-- | For any LVar, we have a generic way to freeze it in a `runParThenFreeze`.
-- instance (DeepFrz a, LVarData1 f) => DeepFrz (f s a) where
--   type FrzType (f s a) = f Frzn a
--   frz = unsafeCoerceLVar

-- ^^^

-- Note that this doesn't work because it CONFLICTS with the other DeepFrz instances.
-- There's no way that we can prove to GHC that pure data will NEVER be an instance
-- of LVarData1, and therefore will never actually cause a conflict with e above
-- instance.
