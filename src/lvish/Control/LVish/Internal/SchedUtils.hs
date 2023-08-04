{-# LANGUAGE CPP            #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo    #-}
{-# LANGUAGE Unsafe         #-}

module Control.LVish.Internal.SchedUtils (
  State(logger, no),
  DbgCfg(..),
  new, number, next, pushWork, nullQ, yieldWork, currentCPU, setStatus, await, prng
  ) where


import           Control.Concurrent
import           Control.Monad
import           Data.Atomics                   (atomicModifyIORefCAS)
import           Data.IORef
import           Prelude
-- import qualified Data.BitList as BL
-- import System.Random (StdGen, mkStdGen)
import           System.Random.PCG.Fast.Pure    (GenIO, initialize)
import           Text.Printf

import qualified System.Log.TSLogger            as L

#ifdef CHASE_LEV
-- #warning "Compiling with Chase-Lev work-stealing deque"

import qualified Data.Concurrent.Deque.ChaseLev as CL

type Deque a = CL.ChaseLevDeque a
newDeque = CL.newQ
pushMine = CL.pushL
popMine  = CL.tryPopL
popOther = CL.tryPopR
pushYield = pushMine -- for now...
nullQ = CL.nullQ

#else
#warning "Compiling with non-scalable deque."
------------------------------------------------------------------------------
-- A nonscalable deque for work-stealing
------------------------------------------------------------------------------

type Deque a = IORef [a]

newDeque = newIORef []

pushMine deque t =
  atomicMod deque $ \ts -> (t:ts, ())

popMine deque = do
  atomicMod deque $ \ts ->
    case ts of
      []      -> ([], Nothing)
      (t:ts') -> (ts', Just t)

nullQ deque = do
  ls <- readIORef deque
  return $! null ls

pushYield deque t =
  atomicMod deque $ \ts -> (ts++[t], ())

popOther = popMine

#endif
-- END: ifdef CHASE_LEV

------------ Signatures, shared by the above: --------------

-- | Create a new local work deque
newDeque :: IO (Deque a)

-- | Add work to a thread's own work deque
pushMine :: Deque a -> a -> IO ()

-- | Take work from a thread's own work deque
popMine :: Deque a -> IO (Maybe a)

nullQ :: Deque a -> IO Bool

-- | Take work from a different thread's work deque
popOther :: Deque a -> IO (Maybe a)

-- | Add low-priority work to a thread's own work deque
pushYield :: Deque a -> a -> IO ()

------------------------------------------------------------


-- The version of atomic modify used in several places in this file:
atomicMod :: IORef a -> (a -> (a, b)) -> IO b
{-# INLINE atomicMod #-}
-- atomicMod = atomicModifyIORef'
atomicMod = atomicModifyIORefCAS


------------------------------------------------------------------------------
-- A scheduling framework
------------------------------------------------------------------------------

-- All the state relevant to a single worker thread
data State a s = State
    { no         :: {-# UNPACK #-} !Int, -- ^ The number of this worker
      numWorkers :: !Int,               -- ^ Total number of workers in this runPar
      prng       :: !GenIO,        -- ^ core-local random number generation
      status     :: !(IORef s),             -- ^ A thread-local flag
      workpool   :: !(Deque a),             -- ^ The thread-local work deque
      idle       :: !(IORef [MVar Bool]),   -- ^ global list of idle workers
      states     :: ![State a s],         -- ^ global list of all worker states.
      logger     :: !(Maybe L.Logger)
        -- ^ The Logger object used by the current Par session, if debugging is activated.
    }

-- | Process the next item on the work queue or, failing that, go into
-- work-stealing mode.
{-# INLINE next #-}
next :: State a s -> IO (Maybe a)
next state@State{ workpool } = do
  e <- popMine workpool
  case e of
    Nothing -> steal state
    Just _t -> return e

-- RRN: Note -- NOT doing random work stealing breaks the traditional
-- Cilk time/space bounds if one is running strictly nested (series
-- parallel) programs.

-- | Attempt to steal work or, failing that, give up and go idle (and then wake back
-- up and keep stealing).
--
--   This function does NOT return until the complete runPar session is complete (all
--   workers idle).
steal :: State a s -> IO (Maybe a)
steal State{ idle, states, no=my_no, numWorkers, logger } = do
  chatter logger $ "!cpu "++show my_no++" stealing"
  go states
  where
    -- After a failed sweep, go idle:
    go [] = do m <- newEmptyMVar
               r <- atomicMod idle $ \is -> (m:is, is)
               if length r == numWorkers - 1
                  then do
                     chatter logger $ printf "!cpu %d initiating shutdown" my_no
                     mapM_ (\mv -> putMVar mv True) r -- Signal to all but us.
                     return Nothing
                  else do
                    chatter logger $ printf "!cpu %d going idle..." my_no
                    done <- takeMVar m
                    if done
                       then do
                         chatter logger $ printf "!cpu %d shutting down" my_no
                         return Nothing
                       else do
                         chatter logger $ printf "!cpu %d woken up" my_no
                         go states
    go (x:xs)
      | no x == my_no = go xs
      | otherwise     = do
         r <- popOther (workpool x)
         case r of
           Just _t -> do
             chatter logger $ printf "cpu %d got work from cpu %d" my_no (no x)
             return r
           Nothing -> go xs

-- | If any worker is idle, wake one up and give it work to do.
pushWork :: State a s -> a -> IO ()
-- TODO: If we're really going to do wakeup on *every* push we could consider giving
-- the formerly-idle worker the work item directly and thus avoid touching the deque.
pushWork State { workpool, idle, logger, no } t = do
  chatter logger $ "Starting pushWork on worker "++show no
  pushMine workpool t
  idles <- readIORef idle
  when (not (null idles)) $ do
    r <- atomicMod idle (\is -> case is of
                                 []      -> ([], return ())
                                 (i:is') -> (is', putMVar i False))
    r -- wake one up

yieldWork :: State a s -> a -> IO ()
yieldWork State { workpool } t =
  pushYield workpool t -- AJT: should this also wake an idle thread?

data DbgCfg
  = DbgCfg
  { dbgDests      :: [L.OutDest]
  , dbgRange      :: Maybe (Int, Int)
  , dbgScheduling :: Bool
  }

-- | Create a new set of scheduler states.
new :: DbgCfg -> Int -> s -> IO (Maybe L.Logger,[State a s])
new DbgCfg{dbgDests,dbgRange,dbgScheduling} numWorkers s = do
  idle   <- newIORef [] -- Shared by all workers.
  let (minLvl, maxLvl) = case dbgRange of
                           Just b  -> b
                           Nothing -> (0,L.dbgLvl)
  let mkLogger = do
         lgr <- L.newLogger (minLvl,maxLvl) dbgDests
                   (if dbgScheduling
                    then L.WaitNum numWorkers countIdle
                    else L.DontWait)
         L.logOn lgr (L.OffTheRecord 1 " [dbg-lvish] Initialized Logger... ")
         return lgr
      -- Atomically count how many workers are currently registered as idle:
      countIdle = do ls <- readIORef idle
                     return $! length ls
  -- Fastpath: if we're not in debug mode don't create the logger at all:
  logger <- if maxLvl > 0
            then fmap Just $ mkLogger
            else return Nothing
  let mkState states i = do
        workpool <- newDeque
        status   <- newIORef s
        prng     <- initialize $ fromIntegral i
        return State { no = i, workpool, idle, status, states, prng, logger, numWorkers }
  rec states <- forM [0..(numWorkers-1)] $ mkState states
  return (logger,states)


number :: State a s -> Int
number State { no } = no

setStatus :: State a s -> s -> IO ()
setStatus State { status } s = writeIORef status s

-- This is a hard-spinning busy-wait.
await :: State a s -> (s -> Bool) -> IO ()
await State { states, logger, no=no1 } p =
  let awaitOne state@(State { status, no=no2 }) = do
        cur <- readIORef status
        unless (p cur) $ do
          case logger of
            Nothing -> return ()
            Just lgr -> L.logOn lgr (L.StrMsg 7 (" [dbg-lvish] busy-waiting on worker "++show no1++
                                                 ", for status to change on worker "++show no2))
          awaitOne state
  in mapM_ awaitOne states

-- | the CPU executing the current thread (0 if not supported)
currentCPU :: IO Int
currentCPU =
#if __GLASGOW_HASKELL__ >= 701 /* 20110301 */
  --
  -- Note: GHC 7.1.20110301 is required for this to work, because that
  -- is when threadCapability was added.
  --
      do
        tid <- myThreadId
        (main_cpu, _) <- threadCapability tid
        return main_cpu
#else
  --
  -- Lacking threadCapability, we always pick CPU #0 to run the main
  -- thread.  If the current thread is not running on CPU #0, this
  -- will require some data to be shipped over the memory bus, and
  -- hence will be slightly slower than the version above.
  --
  return 0
#endif

-- | Local chatter function for this module
chatter :: Maybe L.Logger -> String -> IO ()
-- chatter _ s = putStrLn s -- TEMP...
-- chatter _ s = printf "%s\n" s
-- chatter _ _ = return ()

chatter mlg s = do
  case mlg of
    Nothing -> return ()
    Just lg -> L.logOn lg (L.OffTheRecord 7 s)
