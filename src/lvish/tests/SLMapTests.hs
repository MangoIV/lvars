{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Tests for the Data.LVar.PureMap and Data.LVar.SLMap modules.

module SLMapTests(tests, runTests) where

import qualified Data.Concurrent.SkipListMap as SLM
import qualified Data.LVar.SLMap             as IM
import qualified Data.LVar.SLSet             as IS

import qualified Data.LVar.SLMap             as SM

#include "CommonMapTests.hs"

type TheMap k s v = IM.IMap k s v

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "" [tests_here, tests_common ]

tests_here :: TestTree
tests_here = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain tests

------------------------------------------------------------------------------------------
-- Show instances
------------------------------------------------------------------------------------------

-- | It happens that these come out in the opposite order from the Pure one:
case_show02 :: Assertion
case_show02 = assertEqual "show for SLMap" "{IMap: (\"key2\",44), (\"key1\",33)}" show02
show02 :: String
show02 = show$ runParThenFreeze $ isDet $ do
  mp <- IM.newEmptyMap
  SM.insert "key1" (33::Int) mp
  SM.insert "key2" (44::Int) mp
  return mp

--------------------------------------------------------------------------------
-- Issue related:
--------------------------------------------------------------------------------

-- -- Issue #27, spurious duplication.
-- case_handlrDup :: Assertion
-- case_handlrDup = runParIO $ do
--   ctr <- I.liftIO$ newIORef 0
--   mp  <- SM.newEmptyMap
--   hp  <- newPool
--   -- Register handler FIRST.. no race.
--   SM.forEachHP (Just hp) mp $ \ (k::Int) v -> do
--     logDbgLn 1 $ "[case_handlrDup] Callback executing: " ++ show (k,v)
--     I.liftIO $ incr ctr
--   SM.insert 2 2 mp
--   SM.insert 3 3 mp
--   quiesce hp
--   sum <- I.liftIO $ readIORef ctr
--   I.liftIO $ assertEqual "Should be no duplication in this case" 2 sum

-- incr :: IORef Int -> IO ()
-- incr ref = atomicModifyIORef' ref (\x -> (x+1,()))
