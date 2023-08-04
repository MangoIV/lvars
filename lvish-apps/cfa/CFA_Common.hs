{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module CFA_Common where

import           Control.Applicative            (liftA2, liftA3)
import           Control.DeepSeq
import qualified Control.Monad.State            as State

import           System.Environment             (getEnvironment)
import           System.IO.Unsafe               (unsafePerformIO)
import           System.Mem.StableName          (hashStableName, makeStableName)

import           Control.Applicative
import qualified Data.Map                       as M
import           Data.Time.Clock
import           Debug.Trace
import           System.Random
import           Text.PrettyPrint.GenericPretty (Generic, Out (doc, docPrec))

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     (Test (..))

import           Control.LVish.DeepFrz

-- k-CFA parameters

k_param :: Int
k_param =
  case Prelude.lookup "KPARAM" theEnv of
    Just n  -> trace ("Setting K for K-CFA to: "++n) $
               read n
    Nothing -> 2

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

type Var = String
type Label = Int
data Exp = Halt | Ref Var | Lam Label [Var] Call deriving (Eq, Ord, Show, Generic)
data Call = Call Label Exp [Exp]                 deriving (Eq, Ord, Show, Generic)


instance Out Call
instance Out Exp

instance DeepFrz Exp where
  type FrzType Exp = Exp

instance NFData Exp where
  rnf Halt             = ()
  rnf (Ref v)          = rnf v
  rnf (Lam !l ls call) = seq (rnf ls) (rnf call)

instance NFData Call where
  rnf (Call !l e1 ls) = seq (rnf e1) (rnf ls)

-- Helper functions for constructing syntax trees
-------------------------------------------------

-- | Use a counter to be able to generate unique names (gensyms)
type UniqM = State.State Int

newLabel :: UniqM Int
newLabel = State.state (\i -> (i, i + 1))

newVar :: String -> UniqM String
newVar s = do n <- newLabel
              return (s ++ show n)

freshenNames :: UniqM Call -> UniqM Call
freshenNames act = act >>= loop M.empty
 where
   loop  env (Call lab e1 els) = Call lab <$> loop2 env e1 <*> mapM (loop2 env) els
   loop2 env Halt    = return Halt
   loop2 env (Ref v) = case M.lookup v env of
                         Nothing -> return (Ref v)
                         Just v2 -> return (Ref v2)
   loop2 env (Lam l vs call) =
     do suffs <- mapM (\_ -> newLabel) vs
        let prs = zipWith (\ v suff -> (v, "fresh_"++v++"_"++show suff)) vs suffs
            env' = M.union (M.fromList prs) env
        Lam l (map snd prs) <$> loop env' call

runUniqM :: UniqM a -> a
runUniqM = fst . flip State.runState 0

ref :: Var -> UniqM Exp
ref = return . Ref

lam :: [Var] -> UniqM Call -> UniqM Exp
lam xs c = liftA2 (flip Lam xs) newLabel c

call :: UniqM Exp -> [UniqM Exp] -> UniqM Call
call e es = liftA3 Call newLabel e (sequence es)

call' :: String -> [String] -> UniqM Call
call' e es = call (ref e) (map ref es)

let_ :: Var -> UniqM Exp -> UniqM Call -> UniqM Call
let_ x e c = call (lam [x] c) [e]

halt :: UniqM Exp -> UniqM Call
halt e = call (return Halt) [e]

-- true :: UniqM Exp
-- true = do a <- newVar "left"
--           b <- newVar "right"
--           k <- newVar "kont"
--           lam [a,b,k] $ (call (ref k) [ref a])

-- false :: UniqM Exp
-- false = do a <- newVar "left"
--            b <- newVar "right"
--            k <- newVar "kont"
--            lam [a,b,k] $ (call (ref k) [ref b])

#if 0
true  = ref "true"
false = ref "false"
not_ ls = call (ref "not") ls
#else

true :: UniqM Exp
true = do a <- newVar "left"
          b <- newVar "right"
          k <- newVar "kont"
          lam [a,b,k] $ (call (ref a) [ref k])

false :: UniqM Exp
false = do a <- newVar "left"
           b <- newVar "right"
           k <- newVar "kont"
           lam [a,b,k] $ (call (ref b) [ref k])

not_ :: UniqM Exp -> UniqM Exp -> UniqM Call
not_ bool k1 = do
      k2 <- newVar "kontB"
      a <- newVar "left"
      b <- newVar "right"
      call (k1)
        [lam [a,b,k2] $
         (call (bool)
          [ref b, ref a, ref k2])]
#endif


--------------------------------------------------------------------------------
-- Input Programs for Analysis
--------------------------------------------------------------------------------

-- The Standard Example
--
-- In direct style:
--
-- let id = \x -> x
--     a = id (\z -> halt z)
--     b = id (\y -> halt y)
-- in halt b
standardExample :: UniqM Call
standardExample =
  let_ "id" (lam ["x", "k"] (call (ref "k") [ref "x"])) $
  call (ref "id") [lam ["z"] (halt (ref "z")),
                   lam ["a"] (call (ref "id") [lam ["y"] (halt (ref "y")),
                                               lam ["b"] (halt (ref "b"))])]

-- Example with free varibles (showing escapes):
fvExample :: UniqM Call
fvExample =
  let_ "id" (lam ["x", "k"] (call (ref "k") [ref "x"])) $
  call (ref "id") [lam ["z"] (call (ref "escape") [ref "z"]),
                   lam ["a"] (call (ref "id") [lam ["y"] (call (ref "escape") [ref "y"]),
                                               lam ["b"] (call (ref "escape") [ref "b"])])]

-- | Create repeated copies of an example to use up more time.
scaleExample :: Int -> UniqM Call-> UniqM Call
scaleExample 1 ex = ex
scaleExample n ex =
  let f s = s ++ show n in
  let_ "const" (lam [f "x", f "y", f "k"] (call (ref (f "k")) [ref (f "x")])) $
    call (ref "const") [lam [f "ignore1_"] (scaleExample (n-1) ex),
                        lam [f "ignore2_"] ex,
                        lam [f "z"] (halt (ref (f "z")))]

-- This is very quick for 1..3 but then diverges at 4!
standardBig :: Int -> UniqM Call
standardBig 0 = error "standardBig must take at least 1"
standardBig n =
  let_ "id" (lam ["x", "k"] (call (ref "k") [ref "x"])) $
  loop n (error "shouldn't happen")
  where
    loop 0 a = halt (ref a)
    loop n a = do
      let z = "z"++show n
          a = "a"++show n
      call (ref "id") [lam [z] (halt (ref z)),
                       lam [a] (loop (n-1) a)]


-- boolScramble 0 k = k
boolScramble :: Integral a => a -> UniqM Exp -> UniqM Call
boolScramble 0 k = halt k
boolScramble 1 _ = halt (ref "freeVr")
boolScramble n k = do
  x <- newVar "x"
  y <- newVar "y"
  k1 <- newVar "kA"
  k2 <- newVar "kB"
  let (half,rst) = n `quotRem` 2
      l = lam[x,k1]$ boolScramble half       (ref k1)
      r = lam[y,k2]$ boolScramble (half+rst) (ref k2)
  if n `rem` 2 == 0
  then call false [l,r,k]
  else call true  [l,r,k]

notChain :: Int -> UniqM Exp -> UniqM Exp
notChain 0 k = k -- call k [ref "freeV"]
notChain n k = do
  nxt <- newVar "nxt"
  notChain (n-1) $ lam[nxt] $
   not_ (ref nxt) k

-- randFrom :: Int -> Int
-- randFrom n = fst$ next$ mkStdGen n

-- Look here for more:
-- https://github.com/ilyasergey/reachability/tree/master/benchmarks/gcfa2

blur = mkBlur (lam["fin"] (halt (ref "fin")))

blur2 = mkBlur $ lam["k0"] $
          freshenNames blur

blurN 1 = blur
blurN n = mkBlur $ lam["k0_"++show n] $
            freshenNames (blurN (n-1))

mkBlur :: UniqM Exp -> UniqM Call
mkBlur kont =
  let_ "id"   (lam ["x","k1"]  (call' "k1" ["x"])) $
  let_ "blur" (lam ["y", "k2"] (call' "k2" ["y"])) $
  let_ "lp" (lam ["lp0","a", "n", "k0"]
             (call (ref "leq")
              [ref "a", ref "one",
               lam ["bool"]$
               call (ref "bool")
                 [ lam["k3"] (call' "id" ["a", "k3"])
                 , lam["k4"] body
                 , lam["eta"] (call' _HACK ["eta"])]
              ])) $
    (call (ref "lp")
     [ref "lp", false, ref "two",
      kont])
 where
   body = (call (ref "blur")
           [ref "id",  lam ["rr"] $
            call (ref "rr") [true, lam ["r"] $
            (call (ref "blur")
             [ref "id",  lam ["ss"] $
              call (ref "ss") [false, lam ["s"] $
               (call (ref "blur")
                [ref "lp0", lam ["anon"] $
                 call (ref "sub1") [ref "n", ref "one",
                  lam["n2"]$
                   call (ref "anon") [ref "s", ref "n2",
                    lam ["n3"] $
                     not_ (ref "n3") (ref "k4")
                     ]]])
                ]])
             ]])
   _HACK = "k0" -- This is getting an error (unbound in store)
--   _HACK = "k99" -- hack for now... Hmm, the lvish/inplace one avoids the error.
---------------------------------------------------------
-- (letrec ((id (lambda (x) x))
--          (blur (lambda (y) y))
--          (lp (lambda (a n)
--                (if (<= n 1)
--                    (id a)
--                    (let* ((r ((blur id) #t))
--                           (s ((blur id) #f)))
--                      (not ((blur lp) s (- n 1))))))))
--   (lp #f 2))




-- mj09
--------------------------------------------------------------------------------
-- (let ((h (lambda (b)
-- 	   (let ((g (lambda (z) z)))
-- 	     (let ((f (lambda (k)
-- 			(if b
-- 			    (k 1)
-- 			    (k 2)))))
-- 	       (let ((y (f (lambda (x) x))))
-- 		 (g y)))))))
--   (let ((x (h #t)) (y (h #f)))
--     y))
----------------
-- CPS:


--------------------------------------------------------------------------------

makeMain :: (UniqM Call -> IO ()) -> IO ()
makeMain runExample = defaultMain$ hUnitTestToTests$ TestList $
  [ TestLabel "fvExample"       $ TestCase (runExample fvExample)
  , TestLabel "standardExample" $ TestCase (runExample standardExample)
  , TestLabel "scale1" $ TestCase (runExample$ scaleExample 80 fvExample)
  , TestLabel "scale2" $ TestCase (runExample$ standardBig 4)
  , TestLabel "blur1"   $ TestCase (runExample blur)
  , TestLabel "blur2"  $ TestCase (runExample blur2)

  , TestLabel "simple" $ TestCase $ runExample$
    let_ "fst" (lam ["x","y","k"] (call (ref "k") [ref "x"])) $
    let_ "f" (lam ["z"] (call (ref "z") [ref "z"])) $
    call (ref "fst") [ref "f",
                      ref "fst",
                      lam ["l"] (halt (ref "l"))]

  , TestLabel "omega" $ TestCase $ runExample$
    let_ "f" (lam ["x"] (call (ref "x") [ref "x"])) $
    call (ref "f") [ref "f"]
  ] ++
  [ TestLabel ("boolScramble"++show n) $ TestCase $ runExample $ boolScramble n (ref "freeVr")
  | n <- [0..30] ++ [100,200]
  ]++
  [ TestLabel ("notChain"++show n) $ TestCase $ runExample $ call (notChain n (ref "k0")) [ref "k1"]
  | n <- [300]
  ]++
  [ TestLabel ("blurN_"++show n) $ TestCase (runExample$ blurN n)
--   | n <- [2..8]
  | n <- [2..3] -- Making this smaller so it completes quicker.
  ]


theEnv = unsafePerformIO $ getEnvironment
