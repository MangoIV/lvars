-- | A module with helper functions that are used elsewhere in the LVish repository.
module Data.UtilInternal
  ( traverseWithKey_
  , Traverse_ (..)
  )
where

import Control.Applicative (Applicative, Const (..), pure, (*>))
import Control.Monad (void)
import qualified Data.Map as M
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup ((<>)))
import Prelude ((.))

--------------------------------------------------------------------------------
-- Helper code.
--------------------------------------------------------------------------------

-- Version of traverseWithKey_ from Shachaf Ben-Kiki
-- (See thread on Haskell-cafe.)
-- Avoids O(N) allocation when traversing for side-effect.

newtype Traverse_ f = Traverse_ {runTraverse_ :: f ()}

instance (Applicative f) => Semigroup (Traverse_ f) where
  Traverse_ a <> Traverse_ b = Traverse_ (a *> b)

instance (Applicative f) => Monoid (Traverse_ f) where
  mempty = Traverse_ (pure ())
  mappend = (<>)

-- Since the Applicative used is Const (newtype Const m a = Const m), the
-- structure is never built up.
-- (b) You can derive traverseWithKey_ from myfoldMapWithKey, e.g. as follows:

{-# INLINE traverseWithKey_ #-}
traverseWithKey_ :: (Applicative f) => (k -> a -> f ()) -> M.Map k a -> f ()
traverseWithKey_ f =
  runTraverse_
    . myfoldMapWithKey (\k x -> Traverse_ (void (f k x)))

{-# INLINE myfoldMapWithKey #-}
myfoldMapWithKey :: (Monoid r) => (k -> a -> r) -> M.Map k a -> r
myfoldMapWithKey f = getConst . M.traverseWithKey (\k x -> Const (f k x))
