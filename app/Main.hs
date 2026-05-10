{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

-- | Benchmark: delimited continuation throughput in Circuit.Traced
--
-- Three measurements:
--   1. clock overhead — raw MonotonicRaw resolution
--   2. trace-delim  — Trace (Kleisli IO) Either: iterate until Right
--   3. whileM_      — control group: IORef + whileM_
--
-- Usage:
--   perf-bench --runs 100000 --warmup 1000
--   perf-bench --runs 100000 --core-dump  (dumps GHC Core if built with -ddump-simpl)
module Main where

import Circuit.Perf (Nanos, warmup)
import Circuit.Traced (Trace (..))
import Control.Arrow (Kleisli (..))
import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Control.Monad (replicateM, when)
import Data.IORef
import Data.List qualified as List
import Options.Applicative
import System.Clock (getTime, toNanoSecs, Clock (MonotonicRaw))
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

-- ---------------------------------------------------------------------------
-- CLI
-- ---------------------------------------------------------------------------

data Config = Config
  { cfgRuns   :: !Int
  , cfgWarmup :: !Int
  , cfgDump   :: !Bool
  }

configP :: Parser Config
configP = Config
  <$> option auto (long "runs" <> short 'n' <> value 100000 <> help "Number of iterations per benchmark")
  <*> option auto (long "warmup" <> short 'w' <> value 1000 <> help "Warmup iterations before timing")
  <*> switch (long "core-dump" <> help "Print Core dump flag (for GHC -ddump-simpl)")

-- ---------------------------------------------------------------------------
-- Measurement helpers
-- ---------------------------------------------------------------------------

-- | Single clock read in nanoseconds.
clockRead :: IO Nanos
clockRead = toNanoSecs <$> getTime MonotonicRaw
{-# INLINE clockRead #-}

-- | Measure a single IO action, returning nanoseconds.
--   Forces the result to NF before the second clock read.
measureIO :: NFData a => IO a -> IO Nanos
measureIO action = do
  !t0 <- clockRead
  !result <- action
  evaluateNF result
  !t1 <- clockRead
  pure (t1 - t0)
{-# INLINE measureIO #-}

evaluateNF :: NFData a => a -> IO ()
evaluateNF x = evaluate (rnf x)
{-# INLINE evaluateNF #-}

-- | Run a benchmark N times, return per-iteration nanos (percentiles).
benchmark :: String -> Int -> Int -> IO Nanos -> IO ()
benchmark name warm runs action = do
  putStr $ name <> ": warming up... "
  hFlush stdout
  warmup warm
  putStrLn "done"

  putStr $ name <> ": running " <> show runs <> " iterations... "
  hFlush stdout
  results <- replicateM runs action
  putStrLn "done"

  let sorted = List.sort results
      p10 = sorted !! (runs `div` 10)
      p50 = sorted !! (runs `div` 2)
      p90 = sorted !! (runs * 9 `div` 10)
      avg = sum results `div` fromIntegral runs

  putStrLn $ unwords
    [ "  p10:" , fmt p10
    , "p50:", fmt p50
    , "p90:", fmt p90
    , "avg:", fmt avg
    ]
  where
    fmt n = let (v, u) = scaleNanos n in show (round v :: Int) <> u

-- | Scale nanos to human-readable: returns (value, unit).
scaleNanos :: Nanos -> (Double, String)
scaleNanos n
  | n < 1000    = (fromIntegral n, "ns")
  | n < 1000000 = (fromIntegral n / 1e3, "µs")
  | otherwise   = (fromIntegral n / 1e6, "ms")

-- ---------------------------------------------------------------------------
-- Benchmark 1: clock overhead
-- ---------------------------------------------------------------------------

-- | How long does it take to read the clock twice?
benchClockOverhead :: Int -> Int -> IO ()
benchClockOverhead warm runs = benchmark "clock" warm runs $ do
  !t0 <- clockRead
  !t1 <- clockRead
  pure (t1 - t0)

-- ---------------------------------------------------------------------------
-- Benchmark 2: delimited continuation trace
-- ---------------------------------------------------------------------------

-- | Count from 0 to a target using Trace (Kleisli IO) Either.
--   Each iteration: read the counter, increment if < target, otherwise stop.
--   Internally uses prompt/control0 (delimited continuations).
countWithTrace :: Int -> Kleisli IO (Either Int ()) (Either Int Int)
countWithTrace target = Kleisli \case
  Right () -> countUp 0
  Left  n  -> countUp n
  where
    countUp n
      | n >= target = pure (Right n)
      | otherwise   = pure (Left (n + 1))

runTrace :: Int -> IO Int
runTrace n = runKleisli (trace (countWithTrace n)) ()
{-# NOINLINE runTrace #-}

benchTrace :: Int -> Int -> Int -> IO ()
benchTrace target warm runs = benchmark "trace-delim" warm runs $
  measureIO (runTrace target)

-- ---------------------------------------------------------------------------
-- Benchmark 3: whileM_ (control group)
-- ---------------------------------------------------------------------------

-- | Same counting loop using IORef + whileM_.
--   No delimited continuations — just a mutable cell and a loop.
countWithIORef :: Int -> IO Int
countWithIORef target = do
  ref <- newIORef 0
  let loop = do
        n <- readIORef ref
        if n >= target
          then pure n
          else writeIORef ref (n + 1) >> loop
  loop
{-# NOINLINE countWithIORef #-}

benchWhileM :: Int -> Int -> Int -> IO ()
benchWhileM target warm runs = benchmark "whileM_" warm runs $
  measureIO (countWithIORef target)

-- ---------------------------------------------------------------------------
-- Core dump flag
-- ---------------------------------------------------------------------------

-- | This function exists so GHC can dump Core for it when built with
--   -ddump-simpl -ddump-to-file. It's NOINLINE to prevent GHC from
--   inlining the trace body away.
--
--   Build with:
--     cabal build perf-bench --ghc-options="-ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-type-signatures -fforce-recomp"
--   Output goes to dist-newstyle/build/.../Main.dump-simpl
traceCoreSample :: IO Int
traceCoreSample = do
  runTrace 1000
{-# NOINLINE traceCoreSample #-}

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cfg <- execParser (info (configP <**> helper) fullDesc)
  let runs   = cfgRuns cfg
      warm   = cfgWarmup cfg
      target = 1000  -- fixed iteration count for trace/whileM benchmarks

  when (cfgDump cfg) $ do
    putStrLn "Core dump requested. Rebuild with:"
    putStrLn "  cabal build perf-bench --ghc-options=\"-ddump-simpl -ddump-to-file -dsuppress-all -dno-suppress-type-signatures -fforce-recomp\""
    putStrLn "Then look in dist-newstyle/build/.../Main.dump-simpl for traceCoreSample"
    exitSuccess

  putStrLn $ "perf-bench: runs=" <> show runs <> " warmup=" <> show warm <> " trace-target=" <> show target
  putStrLn ""

  benchClockOverhead warm runs
  putStrLn ""

  benchWhileM target warm runs
  putStrLn ""

  benchTrace target warm runs
  putStrLn ""

  putStrLn "Done."
