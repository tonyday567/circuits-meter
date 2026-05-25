{-# LANGUAGE BlockArguments #-}

-- | Time measurement as a Circuit.
--
-- 'timeM' is the canonical 'Meter' for nanosecond timing. All other
-- time combinators are derived from it via 'Circuit.Meter.meterK' and
-- the 'Trace' iteration structure.
module Circuit.Meter.Time
  ( -- * Time meter
    Nanos,
    nanos,
    timeM,

    -- * Single timing
    tick,
    tickForce,

    -- * Repeated timing
    ticks,
    ticksN,

    -- * Plugin metering
    meterIO,
    meter,

    -- * StepMeasure compatibility
    stepTime,

    -- * Warmup
    warmup,
    warmupK,

    -- * Single-shot measurement runners
    once,
    once_,

    -- * Repeated measurement runners
    timesK,
    timesC,
    times_,
  )
where

import Circuit
import Circuit.Meter
import Control.Arrow
import Control.Category ((.))
import Control.DeepSeq
import Control.Exception
import Control.Monad
import System.Clock
import Prelude hiding (id, (.))

-- ---------------------------------------------------------------------------
-- Clock primitives
-- ---------------------------------------------------------------------------

-- | Nanoseconds as an integral count.
type Nanos = Integer

-- | Read the monotonic raw clock. Absolute value is not meaningful;
-- use deltas between readings.
nanos :: IO Nanos
nanos = toNanoSecs <$> getTime MonotonicRaw
{-# INLINE nanos #-}

-- ---------------------------------------------------------------------------
-- Time meter
-- ---------------------------------------------------------------------------

-- | Read clock before and after; return the delta in nanoseconds.
--
-- >>> import Control.Arrow (Kleisli(..), runKleisli)
-- >>> import Circuit.Meter (meterK)
-- >>> runKleisli (meterK timeM (Kleisli (pure . (*2)))) 5
-- (...,10)
timeM :: Meter IO Nanos Nanos
timeM =
  mkMeter
    nanos
    ( \s -> do
        !e <- nanos
        pure (e - s)
    )
{-# INLINEABLE timeM #-}

-- ---------------------------------------------------------------------------
-- Single timing
-- ---------------------------------------------------------------------------

-- | Measure a single call to a pure function. Forces the result to NF
-- inside the timed 'IO' action so the work cannot be floated out.
once :: (NFData d) => Meter IO a b -> (c -> d) -> c -> IO (b, d)
once m f = runKleisli (meterK m (Kleisli (evaluate . force . f . hold)))
{-# INLINEABLE once #-}

-- | Measure a single call, discarding the result.
once_ :: (NFData d) => Meter IO a b -> (c -> d) -> c -> IO b
once_ m f a = fst <$> once m f a
{-# INLINEABLE once_ #-}

-- | Single timing of a pure function. Returns @(nanos, result)@.
tick :: (NFData b) => (a -> b) -> a -> IO (Nanos, b)
tick = once timeM
{-# INLINEABLE tick #-}

-- | Single timing with deep forcing of argument and result.
tickForce :: (NFData a, NFData b) => (a -> b) -> a -> IO (Nanos, b)
tickForce f a = do
  let !f' = force f
      !a' = force a
  once timeM f' a'
{-# INLINEABLE tickForce #-}

-- ---------------------------------------------------------------------------
-- Repeated timing
-- ---------------------------------------------------------------------------

-- | @n@ timings of the same function. Returns @( [nanos], lastResult )@.
ticks :: (NFData b) => Int -> (a -> b) -> a -> IO ([Nanos], b)
ticks n f = runKleisli (timesC n timeM f)
{-# INLINEABLE ticks #-}

-- | @n@ timings collapsed to a single average nanosecond count.
--
-- The computation is run @n@ times; the total time is divided by @n@.
ticksN :: (NFData b) => Int -> (a -> b) -> a -> IO (Nanos, b)
ticksN n f a = do
  (ts, b) <- ticks n f a
  pure (sum ts `div` fromIntegral n, b)
{-# INLINEABLE ticksN #-}

-- ---------------------------------------------------------------------------
-- Plugin metering
-- ---------------------------------------------------------------------------

-- | Meter an 'IO' action with 'timeM'.
meterIO :: (a -> IO b) -> Circuit (Kleisli IO) (,) a (Nanos, b)
meterIO = meterAction timeM . Kleisli
{-# INLINEABLE meterIO #-}

-- | Meter a pure function with 'timeM'. Forces to NF inside the timed
-- bracket so the work cannot be floated out.
meter :: (NFData b) => (a -> b) -> Circuit (Kleisli IO) (,) a (Nanos, b)
meter f = meterAction timeM (Kleisli (evaluate . force . f . hold))
{-# INLINEABLE meter #-}

-- | 'StepMeasure'-style clock read for interoperability with the
-- @perf@ library pattern.
stepTime :: Meter IO Nanos Nanos
stepTime = timeM
{-# INLINE stepTime #-}

-- ---------------------------------------------------------------------------
-- Warmup
-- ---------------------------------------------------------------------------

-- | Warm up the clock with @n@ dummy reads. Avoids cold-start artefacts.
warmup :: Int -> IO ()
warmup n = replicateM_ n (void nanos)
{-# INLINE warmup #-}

-- | Warmup as a 'Kleisli' circuit. Runs the action @n@ times, then
-- passes the input through unchanged.
--
-- This is the identity circuit with a side-effecting prefix — useful
-- for sequencing warmup before measurement in a pipeline.
warmupK :: Int -> Kleisli IO a a
warmupK n = Kleisli \a -> replicateM_ n (evaluate a) >> pure a
{-# INLINEABLE warmupK #-}

-- ---------------------------------------------------------------------------
-- Repeated measurement runners
-- ---------------------------------------------------------------------------

-- | Measure a 'Kleisli' arrow repeated @n@ times. Returns per-run
-- measurements and the last result.
--
-- The step is marked 'NOINLINE' so GHC cannot float the computation
-- out of the timing loop.
--
-- >>> import Control.Arrow (Kleisli(..))
-- >>> import Circuit.Meter (Meter(..))
-- >>> let m = Meter (Kleisli $ \_ -> pure 0) (Kleisli $ \_ -> pure 0)
-- >>> runKleisli (timesK 3 m (Kleisli (pure . (*2)))) 5
-- ([...,...,...],10)
timesK :: Int -> Meter IO a b -> Kleisli IO c d -> Kleisli IO c ([b], d)
timesK n m k = Kleisli \a -> do
  warmup 100
  let step !x = runKleisli (meterK m k) x
      go 1 !x acc = do
        (t, b) <- step x
        pure (reverse (t : acc), b)
      go i !x acc = do
        (t, _) <- step x
        go (i - 1) x (t : acc)
  go (max 1 n) a []
{-# NOINLINE timesK #-}

-- | Lifted variant of 'timesK' for pure functions. Forces the result
-- to NF inside the timed 'IO' action so the work cannot be floated out.
timesC :: (NFData d) => Int -> Meter IO a b -> (c -> d) -> Kleisli IO c ([b], d)
timesC n m f = timesK n m (Kleisli (evaluate . force . f . hold))
{-# INLINEABLE timesC #-}

-- | Repeated measurement, discarding results.
times_ :: (NFData d) => Int -> Meter IO a b -> (c -> d) -> c -> IO [b]
times_ n m f a = fst <$> runKleisli (timesC n m f) a
{-# INLINEABLE times_ #-}
