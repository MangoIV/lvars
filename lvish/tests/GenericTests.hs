{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tests for the generic Par-programming interfaces.
module GenericTests (tests, runTests) where

import Control.LVish
import Control.Monad
import qualified Control.Par.Class as PC
import Control.Par.Class.Unsafe (internalLiftIO)
import Data.LVar.IVar as IV
import qualified Data.LVar.SLMap as SM
import Data.Maybe (fromMaybe)
import Data.Par.Range (zrange)
import Data.Par.Splittable (pforEach)
import Data.Word
import Test.HUnit
  ( Assertion
  , Counts (..)
  , assertBool
  , assertEqual
  )
import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.TH (testGroupGenerator)
import TestHelpers as T

--------------------------------------------------------------------------------

case_toQPar :: Assertion
case_toQPar = t1 >>= assertEqual "" "hi"

t1 :: IO String
t1 = runParQuasiDet $ isQD par
 where
  --  par :: QuasiDeterministic e => Par e s String
  par = do
    iv <- IV.new
    -- PC.toQPar $
    IV.put iv "hi"
    IV.get iv

--------------------------------------------------------------------------------

size :: Int
size = fromMaybe 100 numElems

expectedSum :: Word64
expectedSum = (s * (s + 1)) `quot` 2
 where
  s = fromIntegral size

-- ParFold instance
case_pfold_imap :: Assertion
case_pfold_imap = assertNoTimeOut 3.0 $ runParNonDet $ isND $ do
  mp <- SM.newEmptyMap
  -- pforEach (zrange sz) $ \ ix -> do
  forM_ [1 .. size] $ \ix -> do
    SM.insert ix (fromIntegral ix :: Word64) mp

  logDbgLn 1 $ "IMap filled up... freezing"
  fmp <- SM.freezeMap mp
  logDbgLn 3 $ "Frozen: " ++ show fmp
  let mapper (_k, x) = do
        logDbgLn 2 $ "Mapping in parallel: " ++ show x
        return x
      folder x y = do
        logDbgLn 2 $ "Summing in parallel " ++ show (x, y)
        return $! x + y
  summed <- PC.pmapFold mapper folder 0 fmp
  logDbgLn 1 $ "Sum of IMap values: " ++ show summed
  internalLiftIO $ assertEqual "Sum of IMap values" expectedSum summed
  return ()

--------------------------------------------------------------------------------

tests :: TestTree
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain tests
