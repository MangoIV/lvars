-- This is NOT a full Haskell module.
-- This is a slice of source code that is #included into multiple files.

-- ASSUMES: module "IM" refers to the Map implementation.

-- This code is for testing write-only operations:

-- import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Control.Concurrent (threadDelay)
-- Some maps need Hashable instead of Ord:

import Control.LVish
import Control.LVish.DeepFrz
  ( DeepFrz (..)
  , Frzn
  , NonFrzn
  , Trvrsbl
  , runParThenFreeze
  , runParThenFreezeIO
  )
import qualified Control.LVish.Internal as I
import Control.Monad (forM, forM_)
import qualified Control.Par.Class as PC
import qualified Data.Foldable as F
import Data.Hashable
import Data.IORef
import Data.LVar.Generic (AFoldable (..), LVarData1 (sortFrzn))
import qualified Data.LVar.IVar as IV
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Traversable (traverse)
import Data.Word
import GHC.Conc (numCapabilities)
import System.Random
import qualified Test.HUnit as HU
import Test.QuickCheck
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.TH (testGroupGenerator)
import TestHelpers2 as T

--------------------------------------------------------------------------------
-- Quickcheck properties:

-- Build simple properties that amount to the identity function, but perform
-- conversions in the middle.
mkSimpleIdentityProp
  :: (Ord v, Ord k, Hashable k, F.Foldable t, DeepFrz a, FrzType a ~ t v)
  => (TheMap k NonFrzn v -> Par ('Ef 'P 'G 'NF 'NB 'NI) NonFrzn a)
  -> [(k, v)]
  -> Bool
mkSimpleIdentityProp trans prs =
  (L.sort $ L.nub $ map snd prs)
    == ( L.sort $
          L.nub $
            F.toList $
              runParThenFreeze $
                isIdemD $ do
                  mp0 <- IM.newFromList prs
                  trans mp0
       )

prop_tofrom :: [Int] -> Bool
prop_tofrom ls = mkSimpleIdentityProp return (zip ls ls)

tests_writeOnly :: TestTree
tests_writeOnly = testGroup "Common" [$ (testGroupGenerator)]

--------------------------------------------------------------------------------

fillNFreezeChunks :: [(Int, Int)] -> TheMap Int Frzn Int
-- fmap IM.freezeMap $
fillNFreezeChunks chunks = runParThenFreeze $ isDet $ do
  mp <- IM.newEmptyMap
  forM chunks $ \(start, end) -> do
    fork $ do
      T.for_ (start, end) $ \n -> IM.insert n n mp
  return mp

fillNFreeze :: Int -> TheMap Int Frzn Int
fillNFreeze sz = fillNFreezeChunks (splitRange numCapabilities (0, sz - 1))

case_fillFreeze1K :: Assertion
case_fillFreeze1K =
  assertEqual
    "fill and then freeze"
    (sum [0 .. sz - 1])
    (case sortFrzn (fillNFreeze sz) of AFoldable x -> F.foldl' (+) 0 x)
 where
  sz = 1000
