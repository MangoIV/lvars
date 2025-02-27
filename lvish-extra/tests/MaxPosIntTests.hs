{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests for the Data.LVar.MaxPosInt module.
module MaxPosIntTests (tests, runTests) where

-- import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))

import Control.Concurrent (killThread, myThreadId)
import Control.LVish hiding (put)
import Control.LVish.DeepFrz (DeepFrz (..), Frzn, Trvrsbl, runParThenFreeze, runParThenFreezeIO)
import qualified Control.LVish.Internal as I
import Data.LVar.MaxPosInt
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

case_mc1 :: Assertion
-- Spuriously failing currently:
-- case_mc1 = assertEqual "mc1" (Just ()) $ timeOutPure 0.3 $ runPar $ do
case_mc1 = assertEqual "mc1" () $ runPar $ do
  num <- newMaxPosInt 0
  fork $ put num 3
  fork $ put num 4
  waitThresh num 4

case_mc2 :: Assertion
case_mc2 = assertEqual "mc2" () $ runPar $ do
  num <- newMaxPosInt 0
  fork $ put num 3
  fork $ put num 4
