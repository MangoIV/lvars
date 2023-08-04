{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- This module reexports the default LVish scheduler, adding some type-level
-- wrappers to ensure propert treatment of determinism.

-- |
--
--   The @lvish@ package provides a parallel programming model based on monotonically
--   growing data structures.
--
--   This module provides the core scheduler and basic control flow
--   operations.  But to do anything useful you will need to import, along
--   with this module, one of the data structure modules (@Data.LVar.*@).
--
--   Here is a self-contained example. This program writes the same value
--   to an @LVar@ called @num@ twice.  It deterministically prints @4@
--   instead of raising an error, as it would if @num@ were a traditional
--   IVar rather than an LVar. (You will need to compile using the
--   @-XDataKinds@ extension.)
--
-- > {-# LANGUAGE DataKinds #-}
-- > import Control.LVish  -- Generic scheduler; works with any lattice.
-- > import Data.LVar.IVar -- The particular lattice in question.
-- >
-- > p :: Par Det s Int
-- > p = do
-- >   num <- new
-- >   fork $ put num 4
-- >   fork $ put num 4
-- >   get num
-- >
-- > main = do
-- >   print $ runPar $ p
module Control.LVish
  ( -- * CRITICAL OBLIGATIONS for the user: valid @Eq@ and total @Ord@

    -- |
    --     We would like to tell you that if you're programming with Safe Haskell (@-XSafe@),
    --     that this library provides a formal guarantee that anything executed with `runPar` is
    --     guaranteed-deterministic.  Unfortunately, as of this release there is still one back-door
    --     that hasn't yet been closed.
    --
    --     If an adversarial user defines invalid `Eq` instances (claiming objects are equal when they're
    --     not), or if they define a `compare` function that is not a /pure, total function/,
    --     and then they store those types within `LVar`s,
    --     then nondeterminism may leak out of a parallel `runPar` computation.
    --
    --     In future releases, we will strive to require alternate, safe versions of `Eq` and
    --     `Ord` that are derived automatically by our library and by the GHC compiler.

    -- * Par computations and their parameters
    Par ()
  , LVishException (..)

    -- * Running various Par computations
  , runPar
  , runParQuasiDet
  , runParNonDet

    -- * More polymorphic variants of same
  , runParPoly
  , runParPolyIO

    -- * Effect signature manipulation and conversion
  , module Control.Par.EffectSigs
  , liftQD

    -- * Combinators for manually constraining the type of a given Par computation
  , isDet
  , isQD
  , isND
  , isIdemD
  , isIdemQD
  , isReadOnly
  , hasPut
  , hasFreeze
  , hasBump
  , hasGet
  , hasIO
  , noPut
  , noFreeze
  , noBump
  , noGet
  , noIO

    -- * Subtyping, add more effects to the signature

    -- | These effects are a conservative approximation, therefore it is always ok,
    --   for example, to turn "no put" (`NP`) into "put" (`P`).
  , addP
  , addG
  , addF
  , addB
  , addI
  , liftReadOnly

    -- * Basic control flow
  , fork
  , yield
  --    quiesceAll,

    -- * Lifting IO, sacrifices determinism
  , parIO

    -- * Various loop constructs
  , parForL
  , parForSimple
  , parForTree
  , parForTiled
  , for_
  , asyncForEachHP

    -- * Logical control flow operators
  , module Control.LVish.Internal.Logical
  -- asyncAnd, asyncOr, andMap, orMap,

    -- * Synchronizing with handler pools
  , L.HandlerPool ()
  , newPool
  , withNewPool
  , withNewPool_
  , quiesce
  , forkHP

    -- * Reexport IVar operations for a full, standard "Par Monad" API
  , module Data.LVar.IVar

    -- * Debug facilities and internal bits
  , logDbgLn
  , dbgChatterOnly
  , getLogger
  , runParLogged
  , runParDetailed
  , OutDest (..)
  , Sch.DbgCfg (..)
  , LVar ()
  )
where

-- NOTE : This is an aggregation module:
import Control.LVish.Internal as I
import Control.LVish.Internal.Basics as B
import Control.LVish.Internal.Logical
-- import           Control.LVish.Internal.Types
import qualified Control.LVish.Internal.SchedIdempotent as L
import qualified Control.LVish.Internal.SchedUtils as Sch
import Control.Par.EffectSigs
import Data.LVar.IVar
import System.Log.TSLogger
  ( LogMsg (..)
  , OutDest (..)
  , logOn
  )

--------------------------------------------------------------------------------

-- | Lifting IO into `Par` in a manner that is fully accounted for in the effect
-- signature.
parIO :: (HasIO e) => IO a -> Par e s a
parIO = I.liftIO

-- | Exactly like `logDbgLn` except used for informational chatter
-- only, NOT to signal that a given thread is about to read or modify
-- a memory address.
dbgChatterOnly :: Int -> String -> Par e s ()
dbgChatterOnly logLvl msg = do
  x <- getLogger
  case x of
    Nothing -> logDbgLn logLvl msg
    Just lgr -> liftIO $ logOn lgr (OffTheRecord logLvl msg)

------------------------------------------------------------

hasPut :: (HasPut e) => Par e s a -> Par e s a
hasPut x = x

hasFreeze :: (HasFreeze e) => Par e s a -> Par e s a
hasFreeze x = x

hasBump :: (HasBump e) => Par e s a -> Par e s a
hasBump x = x

hasIO :: (HasIO e) => Par e s a -> Par e s a
hasIO x = x

hasGet :: (HasGet e) => Par e s a -> Par e s a
hasGet x = x

noPut :: (NoPut e) => Par e s a -> Par e s a
noPut x = x

noFreeze :: (NoFreeze e) => Par e s a -> Par e s a
noFreeze x = x

noBump :: (NoBump e) => Par e s a -> Par e s a
noBump x = x

noIO :: (NoIO e) => Par e s a -> Par e s a
noIO x = x

noGet :: (NoGet e) => Par e s a -> Par e s a
noGet x = x

-- | Deterministic: Everything is switched on but freeze and IO.
isDet :: (e ~ ('Ef 'P 'G 'NF 'B 'NI)) => Par e s a -> Par e s a
isDet x = x

-- | Quasideterministic: Everything is on but IO.
isQD :: (e ~ ('Ef 'P 'G 'F 'B 'NI)) => Par e s a -> Par e s a
isQD x = x

-- | Turn all effect-level switches on, including IO.
isND :: (e ~ ('Ef 'P 'G 'F 'B 'I)) => Par e s a -> Par e s a
isND x = x

-- | Like 'isDet', but with idempotent operations only ('NoBump').
isIdemD :: (e ~ ('Ef 'P 'G 'NF 'NB 'NI)) => Par e s a -> Par e s a
isIdemD x = x

-- | Like 'isQD', but with idempotent operations only ('NoBump').
isIdemQD :: (e ~ ('Ef 'P 'G 'F 'NB 'NI)) => Par e s a -> Par e s a
isIdemQD x = x

-- | Allow "gets" but nothing else.
isReadOnly :: (e ~ ('Ef 'NP 'G 'NF 'NB 'NI)) => Par e s a -> Par e s a
isReadOnly x = x

-- | Lift a read-only computation to participate in a parent computation with more
-- effects.
liftReadOnly :: Par ('Ef 'NP g 'NF 'NB 'NI) s a -> Par ('Ef p g f b i) s a
liftReadOnly (WrapPar p) = WrapPar p

addP :: Par ('Ef 'NP g f b i) s a -> Par ('Ef p g f b i) s a
addP (WrapPar p) = WrapPar p

addG :: Par ('Ef p 'NG f b i) s a -> Par ('Ef p g f b i) s a
addG (WrapPar p) = WrapPar p

addF :: Par ('Ef p g 'NF b i) s a -> Par ('Ef p g f b i) s a
addF (WrapPar p) = WrapPar p

addB :: Par ('Ef p g f 'NB i) s a -> Par ('Ef p g f b i) s a
addB (WrapPar p) = WrapPar p

addI :: Par ('Ef p g f b 'NI) s a -> Par ('Ef p g f b i) s a
addI (WrapPar p) = WrapPar p
