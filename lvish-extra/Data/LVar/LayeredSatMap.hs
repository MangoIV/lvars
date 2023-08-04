{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.LVar.LayeredSatMap
    (
      LayeredSatMap(..)
    , LSMContents(..)
    , forEachHP
    , newEmptyMap
    , newMap
    , newFromList
    , withCallbacksThenFreeze
    , forEach
    , insert
    , pushLayer
    , fromIMap
    , test0
    )
    where

import           Control.LVish.DeepFrz                  (runParThenFreeze)
import           Control.LVish.DeepFrz.Internal
import           Control.LVish.Internal                 as LI
import           Control.LVish.Internal.SchedIdempotent (freezeLV,
                                                         freezeLVAfter, getLV,
                                                         newLV, putLV, putLV_)
import qualified Control.LVish.Internal.SchedIdempotent as L
import           Data.LVar.Generic                      as G
import           Data.LVar.Generic.Internal             (unsafeCoerceLVar)
import qualified Data.LVar.IVar                         as IV
import           Data.UtilInternal                      (traverseWithKey_)

import           Control.LVish

import qualified Data.Foldable                          as F
import           Data.IORef
import qualified Data.Map                               as M
import           Data.Maybe                             (isJust)
import           GHC.Prim                               (unsafeCoerce#)
import           System.IO.Unsafe                       (unsafeDupablePerformIO)

data LayeredSatMap k s v where
    LayeredSatMap :: G.PartialJoinSemiLattice v => (LVar s (LSMContents k v) (k, v)) -> LayeredSatMap k s v

data LSMContents k v = LSMContents [IORef (Maybe (M.Map k v), OnSat)]
                       deriving (Eq)

instance Eq (LayeredSatMap k s v) where
  LayeredSatMap lv1 == LayeredSatMap lv2 = state lv1 == state lv2

instance (Ord k, Ord v) => Ord (LayeredSatMap k Frzn v) where
  compare (LayeredSatMap lv1) (LayeredSatMap lv2) = unsafeDupablePerformIO $ do
    let LSMContents (mpRef1:_) = state lv1
        LSMContents (mpRef2:_) = state lv2
    m1 <- readIORef mpRef1
    m2 <- readIORef mpRef2
    return $ compare (fst m1) (fst m2)

type OnSat = L.Par ()

forEachHP :: Maybe HandlerPool -- ^ optional pool to enroll in
          -> LayeredSatMap k s v -- ^ Map to listen to
          -> (k -> v -> Par d s ()) -- ^ callback
          -> Par d s ()
forEachHP mh (LayeredSatMap (WrapLVar lv)) callback = WrapPar $ do
    L.addHandler mh lv globalCB deltaCB
    return ()
    where
      deltaCB (k, v) = return $ Just $ unWrapPar $ callback k v
      globalCB (LSMContents (mpRef:mps)) = do
        (mp, _) <- L.liftIO $ readIORef mpRef
        case mp of
          Nothing -> return ()
          Just m -> unWrapPar $ traverseWithKey_ (\k v -> forkHP mh $ callback k v) m

newEmptyMap :: G.PartialJoinSemiLattice v => Par d s (LayeredSatMap k s v)
newEmptyMap = newMap M.empty

newMap :: G.PartialJoinSemiLattice v => M.Map k v -> Par d s (LayeredSatMap k s v)
newMap m = WrapPar $ fmap (LayeredSatMap . WrapLVar) $ newLV $ do
  ref <- newIORef (Just m, return ())
  return $ LSMContents [ref]

newFromList :: (Ord k, Eq v, G.PartialJoinSemiLattice v) =>
               [(k,v)] -> Par d s (LayeredSatMap k s v)
newFromList = newMap . M.fromList

withCallbacksThenFreeze :: forall k v b s e . (HasPut e, HasGet e, HasFreeze e, Eq b) =>
                           LayeredSatMap k s v -> (k -> v -> Par e s ()) -> Par e s b -> Par e s b
withCallbacksThenFreeze (LayeredSatMap (WrapLVar lv)) callback action = do
  hp <- newPool
  res <- IV.new
  WrapPar $ freezeLVAfter lv (initCB hp res) deltaCB
  quiesce hp
  IV.get res
      where
        deltaCB (k, v) = return $ Just $ unWrapPar $ callback k v
        initCB hp resIV (LSMContents (mpRef:mps)) = do
          mp <- L.liftIO $ readIORef mpRef
          case mp of
            (Nothing, _) -> return ()
            (Just m, _) -> unWrapPar $ do
               traverseWithKey_ (\k v -> forkHP (Just hp) $ callback k v) m
               res <- action
               IV.put_ resIV res

-- | Flatten this stack of LVars, merging all layers together
-- according the the insert operation, and return the aggregated
-- result map.
fromIMap :: (G.PartialJoinSemiLattice v, Ord k) => LayeredSatMap k Frzn v -> Maybe (M.Map k v)
fromIMap lsm@(LayeredSatMap lv) = unsafeDupablePerformIO $ do
  let LSMContents (mpRef:mps) = state lv
  (mp, onsat) <- readIORef mpRef
  case mp of
    Nothing -> return Nothing
    Just m -> do
      result <- flatten m mps
      return result
  where
    flatten acc [] = return $ Just acc
    flatten acc (p:ps) = do
      parent <- readIORef p
      case parent of
        (Nothing, _) -> return Nothing
        (Just pMap, _) -> case merge acc pMap of
          Nothing -> return Nothing
          Just m  -> flatten m ps
    merge acc m = M.foldWithKey go (Just acc) m
    go _ _ Nothing = Nothing
    go k v (Just m) = case M.lookup k m of
      Nothing -> Just $ M.insert k v m
      Just old -> case joinMaybe old v of
        Nothing     -> Nothing
        Just newVal -> Just $ M.insert k newVal m

instance (Show k, Show v, G.PartialJoinSemiLattice v, Ord k) => Show (LayeredSatMap k Frzn v) where
  show = show . fromIMap

forEach :: LayeredSatMap k s v -> (k -> v -> Par d s ()) -> Par d s ()
forEach = forEachHP Nothing

insert :: (Ord k, G.PartialJoinSemiLattice v, Eq v) =>
          k -> v -> LayeredSatMap k s v -> Par d s ()
insert !key !elm (LayeredSatMap (WrapLVar lv)) = WrapPar $ do
    let LSMContents (mpRef:mps) = L.state lv
    mp <- L.liftIO $ readIORef mpRef
    case mp of
      (Nothing, _) -> return ()
      (Just _, _)  -> putLV_ lv putter
    where
      putter (LSMContents (mpRef:mps)) = do
        (x, act) <- L.liftIO $ atomicModifyIORef' mpRef update
        case act of
          Nothing -> return ()
          (Just a) -> do L.logStrLn 5 $ " [LayeredSatMap] insert saturated lvar" ++ lvid ++ ", running callback."
                         a
                         L.logStrLn 5 $ " [LayeredSatMap] lvar" ++ lvid ++ ", saturation callback completed."
        return (x, ())
      lvid = L.lvarDbgName lv
      delt x = (x, Nothing)
      update n@(Nothing, _) = (n, delt Nothing)
      update orig@(Just m, onsat) = case M.lookup key m of
        Nothing -> ((Just $ M.insert key elm m, onsat), delt $ Just (key, elm))
        Just oldVal -> case joinMaybe elm oldVal of
          Just newVal -> if newVal == oldVal
                         then (orig, delt Nothing)
                         else ((Just $ M.insert key newVal m, onsat), delt $ Just (key, newVal))
          Nothing -> ((Nothing, onsat), (Nothing, Just onsat))

-- | Create a new LVar by pushing a layer onto the existing contents
-- of this map.
pushLayer :: LayeredSatMap k s v -> Par d s (LayeredSatMap k s v)
pushLayer orig@(LayeredSatMap (WrapLVar lv)) = do
  let LSMContents mps = L.state lv
  ref <- WrapPar $ L.liftIO $ newIORef (Just M.empty, return ())
  WrapPar $ fmap (LayeredSatMap . WrapLVar) $ newLV $ return $ LSMContents $ ref:mps

instance DeepFrz a => DeepFrz (LayeredSatMap k s a) where
 type FrzType (LayeredSatMap k s a) = LayeredSatMap k Frzn a -- No need to recur deeper.
 frz = unsafeCoerce# -- Can't use unsafeCoerceLVar due to LVarData1 constraint

test0 :: [Maybe (M.Map String Int)]
test0 = map fromIMap $ runParThenFreeze $ isDet $ do
  m <- newEmptyMap
  insert "foo" (0 :: Int) m
  newLayer1 <- pushLayer m
  newLayer2 <- pushLayer m
  insert "foo" 2 newLayer1
  insert "foo" 1 newLayer2 -- should fail
  insert "bar" 48 m
  return [m, newLayer1, newLayer2]
