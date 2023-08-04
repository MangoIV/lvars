{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tests for the Data.LVar.PureMap and Data.LVar.SLMap modules.
module LayeredSatMapTests (tests, runTests, fillNFreeze) where

import qualified Data.LVar.LayeredSatMap as IM
  ( LayeredSatMap
  , insert
  , newEmptyMap
  , newFromList
  )
import Data.LVar.PureSet as IS

-- TODO: Use backpack for this when it is available:
#include "CommonMapWriteTests.hs"

type TheMap k s v = IM.LayeredSatMap k s v

--------------------------------------------------------------------------------

tests :: Test
tests = testGroup "" [testsHere, tests_writeOnly]

testsHere :: Test
testsHere = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMainSeqTests [tests]

------------------------------------------------------------------------------------------
-- Show instances
------------------------------------------------------------------------------------------

-- case_show03 :: Assertion
-- case_show03 = assertEqual "show for LayeredSatMap" "{LayeredSatMap: (\"key1\",33), (\"key2\",44)}" show03
-- show03 :: String
-- show03 = show$ runParThenFreeze $ do
--   mp <- IM.newEmptyMap
--   IM.insert "key1" (33::Int) mp
--   IM.insert "key2" (44::Int) mp
--   return mp
