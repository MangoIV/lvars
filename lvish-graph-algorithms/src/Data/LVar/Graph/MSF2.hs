{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Minimum spanning forests in LVish
module Data.LVar.Graph.MSF2 where

import Control.LVish
import Control.LVish.Internal (liftIO, unsafeDet)
import Control.Monad
import qualified Data.Foldable as F
import Data.Graph.Adjacency as Adj
import qualified Data.LVar.IStructure as IS
-- import           Data.LVar.PureSet as PS

import qualified Data.LVar.MaxPosInt as MC
import qualified Data.LVar.SLMap as SLM
import Data.LVar.SLSet as PS
import Data.Maybe (fromJust)
import Data.Par.Range (range)
import Data.Par.Splittable (pforEach, pmapReduce_)
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Vector.Unboxed as U hiding (update, (++))

--------------------------------------------------------------------------------

data EdgeGraph = EdgeGraph
  { edges :: (U.Vector (NodeID, NodeID))
  , numVerts :: Int
  }

data MsfState s = MsfState
  { lastSets :: !(V.Vector NodeID)
  , thisSets :: !(IS.IStructure s NodeID)
  , reserves :: !(V.Vector (MC.MaxPosInt s))
  }

main :: IO ()
main = do
  vec <- runParIO $ do
    let fn 0 = (0, 9)
        fn i = (i, i - 1)
        edges = U.generate 10 fn
    liftIO $ putStrLn $ "Starting test with edges: " ++ show edges
    logDbgLn 1 $ "Starting test with edges: " ++ show edges
    -- let is :: IS.IStructure s NodeID
    --     is = undefined
    is <- msf3 EdgeGraph {edges, numVerts = 10}
    IS.freezeIStructure is
  putStrLn $ "Final result: " ++ show vec

msf3 :: EdgeGraph -> Par QuasiDet s (IS.IStructure s ())
msf3 EdgeGraph {edges, numVerts} = do
  -- The state keeps track of which set each vertex is in.  Sets are represented by
  -- an elected leader ID from within the set, where the leader is always the minimum ID.
  let lastSets = V.generate numVerts id
  thisSets <- IS.newIStructure numVerts
  reserves <- newCounters
  output <- IS.newIStructure numEdges

  -- Everyone starts in their own singleton set:
  forSpeculative (0, numEdges) MsfState {lastSets, thisSets, reserves} reserve (commit output) update
  return output
 where
  numEdges = U.length edges
  newCounters = V.generateM numEdges (\_ -> MC.newMaxPosInt 0)

  reserve eid MsfState {lastSets, reserves} = do
    let (a, b) = edges ! eid
        (u, v) = if a > b then (b, a) else (a, b)
        uset = lastSets V.! u
    if u == v
      then return Nothing
      else -- FIXME: must reserve the greater one:
      do
        logDbgLn 3 $ " [msf] bidding for reservation, " ++ show (uset, eid)
        MC.put (reserves V.! uset) eid -- Attempt a reservation.
        return (Just ())

  commit finalOutput eid MsfState {lastSets, thisSets, reserves} () = do
    let (u, v) = edges ! eid
        uset = lastSets V.! u
        vset = lastSets V.! v
    -- Freeze is ok here, because we're after the barrier between reserve/commit.
    winner <- MC.freezeMaxPosInt (reserves V.! uset)
    logDbgLn 3 $ " [msf] Winner for " ++ show uset ++ " was " ++ show winner
    when (winner == eid) $ do
      logDbgLn 3 $ " [msf] WE are the winner, linking: " ++ show (uset, vset)
      IS.put finalOutput eid () -- Mark this edge as included in the final output.
      let newId = min uset vset -- link them together..
      IS.put thisSets uset $! newId
      IS.put thisSets vset $! newId
    return ()

  -- Lots of allocation!:
  update MsfState {thisSets} = do
    -- Freeze the values we wrote in the commit phase.
    vec2 <- IS.freezeIStructure thisSets
    is2 <- IS.newIStructure numEdges
    counters <- newCounters
    let fn ix Nothing = ix
        fn _ (Just nid) = nid
    return $! MsfState (V.imap fn vec2) is2 counters

-- | Inefficient, out-of-place deterministic reservations.
--
--   A barrier separates the reserve, commit, and update phases.
--
--   The update phase enables the creation of "single use" LVars that are frozen in
--   either the reserve or commit rounds and reallocated in the next round.
forSpeculative
  :: (Ord b, Show b)
  => (Int, Int)
  -- ^ Start and end (inclusive/exclusive)
  -> st
  -- ^ Initial state
  -> (Int -> st -> Par e s (Maybe b))
  -- ^ Reserve function
  -> (Int -> st -> b -> Par e s ())
  -- ^ Commit function
  -> (st -> Par e s st)
  -- ^ State update function after each round of commits.
  -> Par e s ()
forSpeculative (st, en) state0 reserve commit update = do
  hp <- newPool
  mainloop hp 0 state0 S.empty 0
 where
  mainloop hp !round state leftover numberDone
    | numberDone == total = return ()
    | otherwise = do
        --     liftIO$ putStrLn $"msf3: inside forSpeculative"

        let remain = total - numberDone
            prefixSize = (min maxRound remain)
            prefixEnd = numberDone + prefixSize
        logDbgLn 2 $
          " [forSpeculative] starting round "
            ++ show round
            ++ ": numDone "
            ++ show numberDone
            ++ ", of "
            ++ show total
            ++ ", prefix "
            ++ show prefixSize

        --     reserved     <- IS.newIStructure prefixSize
        reserved <- IS.newIStructure total -- FIXME: could be prefix size.
        newLeftovers <- newEmptySet

        let doReserve ix = do
              b <- reserve ix state
              case b of
                Nothing -> do
                  logDbgLn 3 $ " [forSpeculative] reserve failed, iter " ++ show ix
                  PS.insert ix newLeftovers
                Just res -> do
                  logDbgLn 3 $ " [forSpeculative] reserve SUCCEEDED, iter " ++ show ix
                  IS.put_ reserved ix res

        -- TODO: do the leftovers in PARALLEL..
        F.foldrM
          ( \ix () -> do
              logDbgLn 3 $ " [forSpeculative] attempting reserve for leftover, iter " ++ show ix
              doReserve ix
          )
          ()
          leftover
        pforEach
          (range numberDone prefixEnd)
          ( \ix -> do
              logDbgLn 3 $ " [forSpeculative] attempting reserve, iter " ++ show ix
              doReserve ix
          )
        leftover' <- unsafeDet $ freezeSet newLeftovers
        {-
             -- Would be nice to have a parallel generate for a BitVector here:
             winners <- pmapReduce_ (range numberDone prefixEnd)
               (\ ix -> do b <- reserve ix state
                           case b of
                             Nothing  -> return S.empty
                             Just res -> return $! S.singleton (ix,res))
               (\ a b -> return $! S.union a b)
               S.empty
             -- FIXME: need good splittable parallel map on sets...
        -}
        reserved' <- unsafeDet $ IS.freezeIStructure reserved
        --     quiesce hp
        logDbgLn 5 $
          " [forSpeculative] finished reserve phase, round "
            ++ show round
            ++ ", new leftovers: "
            ++ show leftover'
            ++ " reserves: "
            ++ show reserved'

        pforEach (range numberDone prefixEnd) $ \ix -> do
          logDbgLn 3 $ " [forSpeculative] checking if we should do commit: " ++ show ix
          case reserved' V.! ix of
            Nothing -> return ()
            Just val -> do
              logDbgLn 3 $ " [forSpeculative] doing commit, iter " ++ show ix
              commit ix state val
        --     quiesce hp
        logDbgLn 3 $ " [forSpeculative] finished commit phase, round " ++ show round
        state' <- update state
        logDbgLn 3 $ " [forSpeculative] finished update phase, round " ++ show round
        let numberDone' = prefixEnd - S.size leftover'
        mainloop hp (round + 1) state' leftover' numberDone'

  total = en - st
  maxRound = max 1 (total `quot` granularity)
  granularity = 20
