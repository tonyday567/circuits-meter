-- | Bare-bones performance measurement. Clock primitives and loop helpers.
--
-- This is the minimal useful subset of the @perf@ library, designed to
-- be composed as a Circuit plugin layer (see @circuits/examples/perf.md@).
-- No Measure, StepMeasure, PerfT, or reporting machinery — just the
-- functions you need to time a tight loop.
module Circuit.Perf
  ( -- * Clock
    Nanos,
    nanos,

    -- * Single measurement
    once,
    once_,

    -- * Repeated measurement
    times,
    times_,

    -- * Warmup
    warmup,
  )
where

import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Control.Monad (replicateM_, void)
import System.Clock (Clock (MonotonicRaw), getTime, toNanoSecs)

-- | Nanoseconds as an integral count.
type Nanos = Integer

-- | Read the monotonic raw clock. Absolute value is not meaningful;
-- use deltas between readings.
nanos :: IO Nanos
nanos = toNanoSecs <$> getTime MonotonicRaw
{-# INLINE nanos #-}

-- | Measure a single call to a pure function. Forces the result to NF.
--
-- @
-- (delta, result) <- once f a
-- @
once :: (NFData b) => (a -> b) -> a -> IO (Nanos, b)
once f a = do
  !t0 <- nanos
  let result = f a
  evaluate (rnf result)
  !t1 <- nanos
  pure (t1 - t0, result)
{-# INLINE once #-}

-- | Measure a single call, discarding the result.
once_ :: (NFData b) => (a -> b) -> a -> IO Nanos
once_ f a = fst <$> once f a
{-# INLINE once_ #-}

-- | Measure @f a@ repeated @n@ times. Returns per-run timings and the
-- last result. Forces each result to NF.
times :: (NFData b) => Int -> (a -> b) -> a -> IO ([Nanos], b)
times n f a = do
  warmup 100
  go n []
  where
    go 0 acc = do
      (delta, result) <- once f a
      pure (reverse (delta : acc), result)
    go k acc = do
      (delta, _) <- once f a
      go (k - 1) (delta : acc)
{-# INLINE times #-}

-- | Measure @f a@ repeated @n@ times, discarding results. Returns per-run timings.
times_ :: (NFData b) => Int -> (a -> b) -> a -> IO [Nanos]
times_ n f a = fst <$> times n f a
{-# INLINE times_ #-}

-- | Warm up the clock with @n@ dummy reads. Avoids cold-start artefacts
-- in the first measurement.
warmup :: Int -> IO ()
warmup n = replicateM_ n (void nanos)
{-# INLINE warmup #-}
