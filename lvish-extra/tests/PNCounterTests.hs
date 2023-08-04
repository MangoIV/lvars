{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests for the Data.LVar.PNCounter module.
module PNCounterTests (tests, runTests) where

-- import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))

import Control.LVish
import Control.LVish.DeepFrz
  ( DeepFrz (..)
  , Frzn
  , Trvrsbl
  , runParThenFreeze
  , runParThenFreezeIO
  )
import qualified Data.LVar.PNCounter as PNC
import qualified Data.Set as S
import qualified Test.HUnit as HU
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.TH (testGroupGenerator)
import TestHelpers as T

--------------------------------------------------------------------------------

tests :: TestTree
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain tests

--------------------------------------------------------------------------------

-- TODO: Write tests.
