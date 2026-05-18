{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

-- | Benchmark: performance measurement as a Circuit.
--
-- Four measurements:
--   1. clock overhead  — raw MonotonicRaw resolution
--   2. whileM_         — control group: IORef + whileM_
--   3. trace-delim     — Trace (Kleisli IO) Either: iterate until Right
--   4. meterK-loop     — same loop measured with 'Circuit.Perf.meterK'
--
-- The last item demonstrates the new API: a 'Meter' wrapped around
-- the delimited-continuation loop, producing per-iteration timings
-- via 'timesK'.
--
-- Usage:
--   perf-bench --runs 100000 --warmup 1000
module Main where

import Circuit.Perf
import Circuit.Perf.Space
import Circuit.Perf.Time
import Circuit.Traced
import Control.Arrow hiding (loop)
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List qualified as List
import GHC.Stats
import Options.Applicative
import System.IO
import Prelude hiding (id, (.))

-- ---------------------------------------------------------------------------
-- CLI
-- ---------------------------------------------------------------------------

data Config = Config
  { cfgRuns :: !Int,
    cfgWarmup :: !Int,
    cfgTraceTarget :: !Int
  }

configP :: Parser Config
configP =
  Config
    <$> option auto (long "runs" <> short 'n' <> value 100000 <> help "Number of outer iterations")
    <*> option auto (long "warmup" <> short 'w' <> value 1000 <> help "Warmup iterations")
    <*> option auto (long "trace-target" <> short 't' <> value 1000 <> help "Inner loop count for trace/whileM")

-- ---------------------------------------------------------------------------
-- Reporting
-- ---------------------------------------------------------------------------

fmt :: Nanos -> String
fmt n = let (v, u) = scaleNanos n in show (round v :: Int) <> u

scaleNanos :: Nanos -> (Double, String)
scaleNanos n
  | n < 1000 = (fromIntegral n, "ns")
  | n < 1000000 = (fromIntegral n / 1e3, "µs")
  | otherwise = (fromIntegral n / 1e6, "ms")

report :: String -> [Nanos] -> IO ()
report name xs = do
  let sorted = List.sort xs
      n = length xs
      p10 = sorted !! (n `div` 10)
      p50 = sorted !! (n `div` 2)
      p90 = sorted !! (n * 9 `div` 10)
      avg = sum sorted `div` fromIntegral n
  putStrLn $
    name
      <> ": p10="
      <> fmt p10
      <> " p50="
      <> fmt p50
      <> " p90="
      <> fmt p90
      <> " avg="
      <> fmt avg

-- ---------------------------------------------------------------------------
-- Benchmark 1: clock overhead
-- ---------------------------------------------------------------------------

benchClock :: Config -> IO [Nanos]
benchClock cfg = do
  let n = cfgRuns cfg
  replicateM n do
    !t0 <- nanos
    !t1 <- nanos
    pure (t1 - t0)

-- ---------------------------------------------------------------------------
-- Benchmark 2: whileM_ control group
-- ---------------------------------------------------------------------------

countIORef :: Int -> IO Int
countIORef target = do
  ref <- newIORef 0
  let loop = do
        n <- readIORef ref
        if n >= target
          then pure n
          else writeIORef ref (n + 1) >> loop
  loop
{-# NOINLINE countIORef #-}

benchWhileM :: Config -> IO [Nanos]
benchWhileM cfg = do
  let target = cfgTraceTarget cfg
      n = cfgRuns cfg
  replicateM n do
    !t0 <- nanos
    !r <- countIORef target
    _ <- evaluate r
    !t1 <- nanos
    pure (t1 - t0)

-- ---------------------------------------------------------------------------
-- Benchmark 3: delimited continuation trace
-- ---------------------------------------------------------------------------

countTrace :: Int -> Kleisli IO (Either Int ()) (Either Int Int)
countTrace target = Kleisli \case
  Right () -> countUp 0
  Left n -> countUp n
  where
    countUp n
      | n >= target = pure (Right n)
      | otherwise = pure (Left (n + 1))
{-# NOINLINE countTrace #-}

runTrace :: Int -> IO Int
runTrace n = runKleisli (trace (countTrace n)) ()
{-# NOINLINE runTrace #-}

benchTrace :: Config -> IO [Nanos]
benchTrace cfg = do
  let target = cfgTraceTarget cfg
      n = cfgRuns cfg
  replicateM n do
    !t0 <- nanos
    !r <- runTrace target
    _ <- evaluate r
    !t1 <- nanos
    pure (t1 - t0)

-- ---------------------------------------------------------------------------
-- Benchmark 4: meterK on the trace loop
-- ---------------------------------------------------------------------------

-- | The trace loop wrapped in a 'Meter'. 'meterK timeM' adds clock
-- reads before and after each call to 'runKleisli (trace ...)'. The
-- 'timesK' combinator then iterates this measured arrow.
benchMeterK :: Config -> IO [Nanos]
benchMeterK cfg = do
  let target = cfgTraceTarget cfg
      n = cfgRuns cfg
      kaction = Kleisli (\() -> runTrace target)
  (ts, _r) <- runKleisli (timesK n timeM kaction) ()
  pure ts

-- ---------------------------------------------------------------------------
-- Benchmark 5: simultaneous time + space (single shot)
-- ---------------------------------------------------------------------------

benchBoth :: Config -> IO ()
benchBoth cfg = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then putStrLn "time+space: skipped (enable with +RTS -T)"
    else do
      let target = cfgTraceTarget cfg
          meter = both timeM allocM
          kaction = Kleisli (\() -> runTrace target)
      ((dt, alloc), _r) <- runKleisli (meterK meter kaction) ()
      putStrLn $
        "time+space: time="
          <> fmt dt
          <> " alloc="
          <> show (unbytes alloc)
          <> "B"

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cfg <- execParser (info (configP <**> helper) fullDesc)
  let runs = cfgRuns cfg
      warm = cfgWarmup cfg
      target = cfgTraceTarget cfg

  putStrLn $ "perf-bench: runs=" <> show runs <> " warmup=" <> show warm <> " trace-target=" <> show target
  putStrLn ""

  -- clock overhead
  putStrLn "1. clock overhead"
  warmup warm
  cs <- benchClock cfg
  report "clock" cs
  putStrLn ""

  -- whileM_ control
  putStrLn "2. whileM_ (IORef control)"
  warmup warm
  ws <- benchWhileM cfg
  report "whileM_" ws
  putStrLn ""

  -- trace-delim
  putStrLn "3. trace-delim (delimited continuations)"
  warmup warm
  ts <- benchTrace cfg
  report "trace-delim" ts
  putStrLn ""

  -- meterK on trace
  putStrLn "4. meterK + timesK (circuit perf API)"
  ms <- benchMeterK cfg
  report "meterK" ms
  putStrLn ""

  -- simultaneous time + space (single shot, not repeated)
  putStrLn "5. both timeM + allocM (single shot)"
  benchBoth cfg
  putStrLn ""

  putStrLn "Done."
