-- #!/usr/bin/env runghc -i..

-- | This module aggregates all the unit tests in this directory.
module Main where

-- import qualified LayeredSatMapTests
import qualified AddRemoveSetTests
import qualified ArrayTests
import qualified MaxPosIntTests
import qualified MemoTests
import qualified SatMapTests
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain alltests

-- alltests :: [TestTree]
alltests :: TestTree
alltests =
  testGroup
    "allTests"
    [ ArrayTests.tests
    , MemoTests.tests
    , MaxPosIntTests.tests
    , SatMapTests.tests
    , --       , LayeredSatMapTests.tests
      AddRemoveSetTests.tests
    ]
