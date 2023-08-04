{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TypeFamilies    #-}

{-|

A mechanism to describe iteration spaces supporting parallel execution.

-}

module Data.Par.Range
  (
    -- * Constructing iteration spaces

    -- | Beware that ranges include information about whether or not sequential
    -- "bottoming-out" is permitted.  If you are executing parallel loops with
    -- inter-iteration dependencies, be careful to use `fullpar`.
    Range(..), range, irange, zrange, fullpar,

    -- * Combined MapReduce operations on ranges
    pmapReduce, pmapReduce_,

    -- * Misc
    toList
  )
where

import           Control.DeepSeq
import           Control.Monad          as M hiding (join, mapM, sequence)
import           Data.Traversable       ()
import           GHC.Conc               (getNumProcessors)

import           Control.Par.Class
import           Control.Par.EffectSigs
import           Data.Splittable.Class  as Sp
import           Prelude                hiding (head, init, max, sequence, tail)

import           System.IO.Unsafe       (unsafePerformIO)
-- import qualified Data.Par.Splittable as P

-- --------------------------------------------------------------------------------

-- | An iteration space expressed as an inclusive range of integers,
-- i.e. `InclusiveRange 1 3` includes both `1` and `3`.
--
--   An iteration space also includes information that determines splitting behavior
--   (e.g. threshold to bottom out to sequential execution).
data Range =
     InclusiveRange
     { startInd  :: {-# UNPACK #-} !Int -- ^ Start, inclusive
     , endInd    :: {-# UNPACK #-} !Int -- ^ End, inclusive
     , seqThresh :: {-# UNPACK #-} !Int -- ^ For ranges less than or equal to this, switch to sequential execution.
     }
--     | TiledRange -- Sequential spawning of tiles rather than tree-spawning.
--                     This creates more "left-biased" executions.
--                     (Tiles internally are just InclusiveRange's)
--     | ??
     deriving (Eq,Ord,Show,Read)

instance Split Range where
  {-# INLINE split #-}
  split = splitPlease 2
  {-# INLINE splitPlease #-}
  splitPlease pieces rng@(InclusiveRange start end thresh)
     | len <= thresh = [rng]
     | len < pieces  = [ InclusiveRange i i thresh | i <- [start .. end]]
     | otherwise = chunks
    where
    len = end - start + 1
    chunks =
      map largepiece [0..remain-1] ++
      map smallpiece [remain..pieces-1]
    (portion, remain) = len `quotRem` pieces
    largepiece i =
        let offset = start + (i * (portion + 1))
        in (InclusiveRange offset (offset + portion) thresh)
    smallpiece i =
        let offset = start + (i * portion) + remain
        in (InclusiveRange offset (offset + portion - 1) thresh)

instance Generator Range where
  type ElemOf Range = Int
  {-# INLINE fold #-}
  fold fn inita (InclusiveRange st en _thresh) =
    loop inita st
    where
      loop !acc i | i > en   = acc
                  | otherwise = loop (fn acc i) (i+1)
  {-# INLINE foldM #-}
  foldM fn !inita (InclusiveRange st en _thresh) =
    forAcc_ st en inita (flip fn)

  {-# INLINE foldMP #-}
  foldMP fn !inita (InclusiveRange st en _thresh) =
    forAcc_ st en inita (flip fn)

  {-# INLINE forM_ #-}
  forM_ (InclusiveRange st en _thresh) fn = for_ st en fn

  {-# INLINE forMP_ #-}
  forMP_ (InclusiveRange st en _thresh) fn = for_ st en fn

-- | Enumerate the elements in a Range.
toList :: Range -> [Int]
toList (InclusiveRange st en _) = [st..en]

-- | A simple shorthand for ranges from `n` to `m-1` (inclusive,exclusive).
--
-- Note that this function, as well as `irange` and `zrange`, by default produce
-- \"auto-sequentializing\" iteration spaces that choose a threshold for bottoming
-- out to sequential execution based on the size of the range and the number of
-- processors.
range :: Int -> Int -> Range
range s e = mkInclusiveRange s (e-1)
{-# INLINE range #-}

-- | A simple shorthand for inclusive ranges from `n` to `m`.
irange :: Int -> Int -> Range
irange s e = mkInclusiveRange s e
{-# INLINE irange #-}

-- | A simple shorthand for ranges from `0` to `n-1`
zrange :: Int -> Range
zrange n = mkInclusiveRange 0 (n-1)
{-# INLINE zrange #-}

-- | Tweak an iteration range to exploit all parallelism.  That is, never bottom-out
-- to sequential loops.
fullpar :: Range -> Range
fullpar (InclusiveRange s e _) = InclusiveRange s e 1
{-# INLINE fullpar #-}

-- By default we produce a range that bottoms out to sequential.
mkInclusiveRange :: Int -> Int -> Range
mkInclusiveRange startInd endInd = InclusiveRange {startInd,endInd,seqThresh}
  where
    seqThresh = min max_iterations_seq chunksize
    chunksize = len `quot` (auto_partition_factor * num_procs)
    len = endInd - startInd + 1
{-# INLINE mkInclusiveRange #-}

--------------------------------------------------------------------------------
-- Parallel granularity heuristics... this could be replaced with auto-tuning.
--------------------------------------------------------------------------------

-- How many tasks per process should we aim for?  Higher numbers
-- improve load balance but put more pressure on the scheduler.
auto_partition_factor :: Int
auto_partition_factor = 4

-- Running large numbers of iterations will take time even if the work is trivial.
-- Thus we put an upper bound on how large we will make a sequential slab of work.
max_iterations_seq :: Int
max_iterations_seq = 4000

-- | We used to be able to use `numCapabilities` for this.  But now, with
-- `numCapabilities` changing at runtime, it will become a source of nondeterminism.
num_procs :: Int
-- num_procs = numCapabilities -- When will this constant version be deprecated/removed?
num_procs = unsafePerformIO getNumProcessors



--------------------------------------------------------------------------------

-- | For convenience: A variant of `Data.Par.Splittable.pmapReduce`
-- specialized to operating on `Range` data.
pmapReduce
   :: (NFData a, ParFuture p, HasPut e, HasGet e, FutContents p a)
      => Range   -- ^ iteration range over which to calculate
      -> (Int -> p e s a)     -- ^ compute one result
      -> (a -> a -> p e s a)  -- ^ combine two results
      -> a                -- ^ initial result
      -> p e s a
-- pmapReduce = P.pmapReduce
pmapReduce = mkMapReduce spawn

-- | A version of `pmapReduce` that is only weak-head-normal-form (WHNF) strict in
-- the folded accumulators.
--
-- Also, like `pmapReduce`, this is merely a specialized version of
-- `Data.Par.Splittable.pmapReduce_`.
pmapReduce_
   :: (ParFuture p, HasPut e, HasGet e, FutContents p a)
      => Range   -- ^ iteration range over which to calculate
      -> (Int -> p e s a)     -- ^ compute one result
      -> (a -> a -> p e s a)  -- ^ combine two results
      -> a                -- ^ initial result
      -> p e s a
pmapReduce_ = mkMapReduce spawn_




-- TODO: Replace with generic version:
{-# INLINE mkMapReduce #-}
mkMapReduce :: (ParFuture m, HasPut e,  HasGet e, FutContents m t) =>
               (m e s t -> m e s (Future m s t)) ->
               Range -> (Int -> m e s t) -> (t -> t -> m e s t) -> t -> m e s t
mkMapReduce spawner irng fn binop init = loop irng
 where
  mapred b ac = do x <- fn b;
                   result <- ac `binop` x
                   return result
  loop rng =
    case split rng of
      -- Sequential case:
      [InclusiveRange st en _] -> forAcc_ st en init mapred
        -- foldM mapred init [min..max]
      [a,b] -> do iv <- spawner$ loop a
                  res2 <- loop b
                  res1 <- get iv
                  binop res1 res2
      ls@(_:_:_) -> do ivs <- mapM (spawner . loop) ls
                       M.foldM (\ acc iv -> get iv >>= binop acc) init ivs
      [] -> return init


{-
-- OLD Lvish versions...

-- | Parallel for-loop over an inclusive range.  Semantically similar to:
--
-- > parFor (InclusiveRange n m) f = forM_ (randomize_order [n..m]) f
--
-- The implementation will split the work into an unspecified number of subtasks in an
-- attempt to gain parallelism.  The exact number of subtasks is chosen at runtime,
-- and is probably a small multiple of the available number of processors.
--
-- Strictly speaking the semantics of 'parFor' depends on the number of processors,
-- and its behaviour, while deterministic, is a function of which machine it is run
-- on.  (Note that this is true even for sequential Haskell programs, because the
-- size of `Int` varies between platforms.)
--
-- The a correct use of `parFor` not have any iteration block on any other iteration.
-- (As if using a sequential for-loop but one with a random order.)
--
parFor :: (ParFuture iv p) => InclusiveRange -> (Int -> p ()) -> p ()
parFor (InclusiveRange start end) body =
 do
    let run (x,y) = for_ x (y+1) body
        range_segments = splitInclusiveRange (4*numCapabilities) (start,end)

    vars <- M.forM range_segments (\ pr -> spawn_ (run pr))
    M.mapM_ get vars
    return ()



{-# INLINE parForSimple #-}
-- | The least-sophisticated form of parallel loop.  Fork all iterations,
-- immediately, as individual parallel tasks.
--
-- When using this kind of loop, it is safe for iterations to do depend on eachother
-- and communicate via blocking reads.  As long as there are no cycles, the runtime
-- will figure out what order to execute the tasks to satisify their data dependency.
parForEach :: (Int,Int) -> (Int -> Par e s ()) -> Par e s ()
parForEach range fn = for_ range $ \i -> fork (fn i)

-}

-- data LRange =
{-

{-# INLINE parForL #-}
-- | Left-biased parallel for loop.  As worker threads beyond the first are added,
-- this hews closer to the sequential iteration order than an unbiased parallel loop.
--
-- Takes a range as inclusive-start, exclusive-end.
parForL :: (Int,Int) -> (Int -> Par e s ()) -> Par e s ()
parForL (start,end) _ | start > end = error$"parForL: start is greater than end: "++show (start,end)
parForL (start,end) body = do
  -- logStrLn$ " initial iters: "++show (end-start)
  loop 0 (end - start) 1
 where
   loop offset remain chunk
     | remain <= 0     = return ()
     | remain <= chunk = parForSimple (offset, offset+remain) body
     | otherwise       = do
         let nxtstrt = offset+chunk
         -- logStrLn$ "loop:  .. "++show (offset, remain, chunk)
         fork$ parForSimple (offset, nxtstrt) body
         loop nxtstrt (remain-chunk) (2*chunk)


-- | Divide the iteration space recursively, but ultimately run every iteration in
-- parallel.  That is, the loop body is permitted to block on other iterations.
parForTree :: (Int,Int) -> (Int -> Par e s ()) -> Par e s ()
parForTree (start,end) _
  | start > end = error$"parForTree: start is greater than end: "++show (start,end)
parForTree (start,end) body = do
  loop 0 (end - start)
 where
   loop offset remain
     | remain == 1     = body offset
     | otherwise       = do
         let (half,rem) = remain `quotRem` 2
         fork$ loop offset half
         loop (offset+half) (half+rem)


-- | Split the work into a number of tiles, and fork it in a tree topology.
parForTiled :: Int -> (Int,Int) -> (Int -> Par e s ()) -> Par e s ()
parForTiled otiles (start,end) body = do
  loop 0 (end - start) otiles
 where
   loop offset remain tiles
     | remain == 1     = body offset
     | tiles  == 1     = for_ (offset,offset+remain) body
     | otherwise       = do
         let (half,rem)   = remain `quotRem` 2
             (halfT,remT) = tiles `quotRem` 2
         fork$ loop offset half halfT
         loop (offset+half) (half+rem) (halfT+remT)


-- | A simple for loop for numeric ranges (not requiring deforestation
-- optimizations like `forM`).  Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => (Int, Int) -> (Int -> m ()) -> m ()
for_ (start, end) _fn | start > end = error "for_: start is greater than end"
for_ (start, end) fn = loop start
  where
  loop !i | i == end  = return ()
          | otherwise = do fn i; loop (i+1)

-}


-- My own forM for numeric ranges (not requiring deforestation optimizations).
{-# INLINE for_ #-}
for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for_ start end _fn | start > end = error "for_: start is greater than end"
for_ start end fn = loop start
  where
   loop !i | i > end  = return ()
           | otherwise = do fn i; loop (i+1)

-- | Inclusive / Inclusive
{-# INLINE forAcc_ #-}
forAcc_ :: Monad m => Int -> Int -> acc -> (Int -> acc -> m acc) -> m acc
forAcc_ start end _ _fn | start > end = error "for_: start is greater than end"
forAcc_ start end acc fn = loop acc start
  where
   loop !ac !i
     | i > end  = return ac
     | otherwise = do acc' <- fn i ac
                      loop acc' (i+1)
