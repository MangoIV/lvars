{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests for SNZI data structure.
module SNZITests where

-- [2013.09.26] Temporarily disabling template haskell due to GHC bug discussed here:
--   https://github.com/rrnewton/haskell-lockfree/issues/10

-- import TestHelpers (defaultMainSeqTests)
-- import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (SomeException, catch, evaluate)
import Control.LVish
import Control.LVish.DeepFrz (DeepFrz (..), Frzn, Trvrsbl, runParThenFreeze, runParThenFreezeIO)
import qualified Control.LVish.Internal as I
import Control.Monad
import qualified Data.Concurrent.LinkedMap as LM
import qualified Data.Concurrent.SNZI as SNZI
import qualified Data.Concurrent.SkipListMap as SLM
import Data.IORef
import qualified Data.LVar.Generic as G
import qualified Data.LVar.IStructure as ISt
import qualified Data.LVar.IVar as IV
import Data.LVar.Memo as Memo
import qualified Data.LVar.NatArray as NA
import qualified Data.LVar.Pair as IP
import Data.LVar.PureMap as IM
import Data.LVar.PureSet as IS
import qualified Data.LVar.SLMap as SM
import qualified Data.LVar.SLSet as SS
import Data.List (intersperse, isInfixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Clock
import Data.Traversable (traverse)
import qualified Data.Vector as V
import Data.Word
import GHC.Conc
import Internal.Control.LVish.SchedIdempotent (dbgLvl, forkWithExceptions, liftIO)
import qualified Internal.Control.LVish.SchedIdempotent as L
import System.Environment (getArgs)
import System.Exit
import System.IO
import System.Random
import qualified Test.HUnit as HU
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)
import TestHelpers as T

--------------------------------------------------------------------------------
-- TESTS FOR SNZI
--------------------------------------------------------------------------------

-- | Test snzi in a sequential setting
snzi1 :: IO (Bool)
snzi1 = do
  (cs, poll) <- SNZI.newSNZI
  forM_ cs SNZI.arrive
  forM_ cs SNZI.arrive
  forM_ cs SNZI.depart
  forM_ cs SNZI.depart
  poll

case_snzi1 :: Assertion
case_snzi1 = snzi1 >>= assertEqual "sequential use of SNZI" True

-- | Very simple sequential snzi test
snzi2a :: IO (Bool)
snzi2a = do
  (cs, poll) <- SNZI.newSNZI
  forM_ cs SNZI.arrive
  poll

case_snzi2a :: Assertion
case_snzi2a = snzi2a >>= assertEqual "sequential use of SNZI" False

-- | Test snzi in a sequential setting
snzi2 :: IO (Bool)
snzi2 = do
  (cs, poll) <- SNZI.newSNZI
  forM_ cs SNZI.arrive
  forM_ cs SNZI.arrive
  forM_ cs SNZI.depart
  forM_ cs SNZI.depart
  forM_ cs SNZI.arrive
  poll

case_snzi2 :: Assertion
case_snzi2 = snzi2 >>= assertEqual "sequential use of SNZI" False

-- | Test snzi in a concurrent setting
snzi3 :: IO (Bool)
snzi3 = do
  (cs, poll) <- SNZI.newSNZI
  mvars <- forM cs $ \c -> do
    mv <- newEmptyMVar
    forkWithExceptions forkIO "snzi3 test thread" $ do
      nTimes 1000000 $ \_ -> do
        SNZI.arrive c
        SNZI.depart c
        SNZI.arrive c
        SNZI.arrive c
        SNZI.depart c
        SNZI.depart c
      putMVar mv ()
    return mv
  forM_ mvars takeMVar
  poll

case_snzi3 :: Assertion
case_snzi3 = snzi3 >>= assertEqual "concurrent use of SNZI" True

-- | Test snzi in a concurrent setting
snzi4 :: IO (Bool)
snzi4 = do
  (cs, poll) <- SNZI.newSNZI
  mvars <- forM cs $ \c -> do
    mv <- newEmptyMVar
    internalMV <- newEmptyMVar
    forkWithExceptions forkIO "snzi4 test thread type A" $ do
      SNZI.arrive c
      putMVar internalMV ()
    forkWithExceptions forkIO "snzi4 test thread type B" $ do
      nTimes 1000000 $ \_ -> do
        SNZI.arrive c
        SNZI.depart c
        SNZI.arrive c
        SNZI.arrive c
        SNZI.depart c
        SNZI.depart c
      takeMVar internalMV
      putMVar mv ()
    return mv
  forM_ mvars takeMVar
  poll

case_snzi4 :: Assertion
case_snzi4 = snzi4 >>= assertEqual "concurrent use of SNZI" False

--------------------------------------------------------------------------------

tests :: TestTree
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain tests
