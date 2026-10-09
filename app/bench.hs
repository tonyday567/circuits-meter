-- | circuits-meter-bench — the thc speed-test artifact (loom/kmett.md,
-- "the speed question"), run today under stock GHC for the baseline.
--
-- Three workloads picked to stress what thc/jam claim to be good at
-- (allocation churn, pointer density, lazy thunk machinery), held to
-- the thc-compatible subset: base + deepseq + circuits + circuits-rel
-- only, no FFI, no text/bytestring:
--
--   1. gf2     — rrefRows over growing Bool matrices (Circuit.Cellular):
--                pointer- and allocation-heavy dense list matrices.
--   2. netmelt — build an SMC/Bimonoid Net and run (normalize) it
--                (Circuit.Net/Circuit.Syntax): constructor churn.
--   3. moore   — a long lazy list through scan . moore
--                (Circuit.GMachine): closure/thunk machinery.
--
-- Every workload prints a deterministic checksum alongside the timing,
-- so the work is genuinely consumed and a future thc run must produce
-- the same checksums — the cross-compiler correctness oracle.
--
-- Sizes (GHC 9.14.1, aarch64-darwin baseline; each run lands in the
-- 10ms-2s band):
--
--   gf2:     matrices 120/160/200, reps 3
--   netmelt: chains of 20000/40000 superblocks, reps 3
--   moore:   streams of 300000/600000 ints, reps 3
module Main (main) where

import Circuit
import Circuit.Cellular (rrefRows)
import Circuit.Meter.Time (ticksN)
import Control.DeepSeq (NFData, force)
import Prelude hiding (id, (.))

-- * harness

-- | Time a workload with the meter's own runner: 'ticksN' warms the
-- clock, runs @reps@ times, and averages.  The workload deep-forces
-- its result before returning the checksum, so the timed call
-- includes the full evaluation.
bench :: (NFData r, Show r) => String -> Int -> Int -> (Int -> r) -> [Int] -> IO ()
bench label reps width f sizes = mapM_ runOnce sizes
  where
    runOnce n = do
      (t, r) <- ticksN reps (\input -> force (f input)) n
      putStrLn (pad label ++ " n=" ++ show n ++ " avg=" ++ show t ++ "ns reps=" ++ show reps ++ " checksum=" ++ show r)
    pad s = take width (s ++ repeat ' ')

-- * workload 1: GF(2) linear algebra

-- | Deterministic dense Bool matrix: two warmup rounds of the MMIX
-- LCG on the linear site index, high bit read out — no random
-- package, thc-compatible, full rank diversity.
genMatrix :: Int -> [[Bool]]
genMatrix n = [[mixBit (i * n + j) | j <- [0 .. n - 1]] | i <- [0 .. n - 1]]

mixBit :: Int -> Bool
mixBit k = odd (lcg (lcg (fromIntegral k + 1)) `div` ((2 :: Integer) ^ 32))

lcg :: Integer -> Integer
lcg x = (x * 6364136223846793005 + 1442695040888963407) `mod` ((2 :: Integer) ^ 64)

-- | rref plus a popcount checksum of the reduced rows.
gf2 :: Int -> (Int, Int)
gf2 n =
  let rr = rrefRows (genMatrix n)
   in (length rr, sum (map (length . filter id) rr))

-- * workload 2: Net melt

-- | Structural nodes at type (,) (->) Int, as direct 'Oper' towers.
nPar :: Net (,) (->) a c -> Net (,) (->) b d -> Net (,) (->) (a, b) (c, d)
nPar f g = Oper (R (L (SigPar f g)))

nSwap :: Net (,) (->) (a, b) (b, a)
nSwap = Oper (R (R (L SigSwap)))

nCopy :: (CopyT (,) (->) a) => Net (,) (->) a (a, a)
nCopy = Oper (R (R (R (L SigCopy))))

nDiscard :: (DiscardT (,) (->) a) => Net (,) (->) a ()
nDiscard = Oper (R (R (R (R (L SigDiscard)))))

-- | One superblock: compute, fan out through the bimonoid copy row,
-- drain one wire through discard, revive it with a constant while the
-- other wire keeps carrying — so the output is a nontrivial function
-- of the whole chain.  Exercises SigCompose, SigPar, SigSwap, SigCopy,
-- SigDiscard in one (Int,Int) -> (Int,Int) loop.
superblock :: Int -> Net (,) (->) (Int, Int) (Int, Int)
superblock k =
  nPar (Lift (+ (k `mod` 3))) (Lift (const (k `mod` 5)))
    . nPar (Lift id) nDiscard
    . nPar (Lift (uncurry (+))) (Lift (uncurry (+)))
    . nPar nCopy nCopy
    . nPar (Lift (subtract 3)) (Lift (+ 7))
    . nSwap
    . nPar (Lift (* 2)) (Lift (`div` 2))
    . nSwap

-- | A chain of superblocks: the constructor-churn payload.
chainNet :: Int -> Net (,) (->) (Int, Int) (Int, Int)
chainNet k = foldr (.) id [superblock i | i <- [1 .. k]]

-- | Normalize the net and observe it on three seeds.
netmelt :: Int -> ((Int, Int), (Int, Int), (Int, Int))
netmelt k =
  let net = chainNet k
   in ( run net (1, 2),
        run net (37, 41),
        run net (k, k + 1)
      )

-- * workload 3: Moore stream fold

-- | A long lazy list through a Moore machine, fully forced.
mooreStream :: Int -> (Int, Int, Int)
mooreStream n =
  let os = scan (moore (+ 0) (\s a -> s + a) id) [1 .. n]
   in (length os, sum os, last os)

-- * main

main :: IO ()
main = do
  putStrLn "circuits-meter-bench (GHC baseline; thc numbers to follow)"
  bench "gf2" 3 12 gf2 [120, 160, 200]
  bench "netmelt" 3 12 netmelt [20000, 40000]
  bench "moore" 3 12 mooreStream [300000, 600000]
