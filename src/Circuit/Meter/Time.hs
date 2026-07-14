{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

-- | Time measurement as a Circuit.
--
-- 'timeX' is the canonical 'Meter' for nanosecond timing. All other
-- time combinators are derived from it via 'Circuit.Meter.meterAction'.
module Circuit.Meter.Time
  ( -- * Time meter
    Nanos,
    nanos,
    timeX,

    -- * Single timing
    tick,

    -- * Repeated timing
    ticks,
    ticksN,

    -- * IO repeated timing
    ticksION,

    -- * Plugin metering
    meterIO,
    meter,

    -- * Warmup
    warmup,

    -- * Reify helper
    reifyC,

    -- * Single-shot measurement runners
    once,
    onceK,

    -- * Repeated measurement runners
    timesK,
  )
where

import Circuit
import Circuit.Classes (Category (..), Discrete (..))
import Circuit.Meter
import Control.Arrow
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Fix
import System.Clock
import Prelude hiding (id, (.))

-- ---------------------------------------------------------------------------
-- Clock primitives
-- ---------------------------------------------------------------------------

-- | Nanoseconds as an integral count.
type Nanos = Integer

-- | Read the monotonic clock. Absolute value is not meaningful;
-- use deltas between readings.
--
-- On Linux/macOS we use 'MonotonicRaw' for NTP-frequency-adjustment-free
-- timing; on Windows we fall back to the portable 'Monotonic' clock.
nanos :: IO Nanos
#ifdef mingw32_HOST_OS
nanos = toNanoSecs <$> getTime Monotonic
#else
nanos = toNanoSecs <$> getTime MonotonicRaw
#endif
{-# INLINE nanos #-}

-- ---------------------------------------------------------------------------
-- Time meter
-- ---------------------------------------------------------------------------

-- | Read clock before and after; return the delta in nanoseconds.
--
-- This is a stopwatch: 'start' captures the initial time, and each
-- 'stop' reads the current clock and subtracts.  Calling 'stop' multiple
-- times gives cumulative elapsed time since the single start.
timeX :: Meter (Kleisli IO) Nanos Nanos
timeX =
  mkMeter
    nanos
    ( \s -> do
        !e <- nanos
        pure (e - s)
    )
{-# INLINEABLE timeX #-}

-- ---------------------------------------------------------------------------
-- Single timing
-- ---------------------------------------------------------------------------

-- | Measure a single call to a pure function. Forces the result to NF
-- inside the timed 'IO' action so the work cannot be floated out.
once :: (NFData d) => Meter (Kleisli IO) a b -> (c -> d) -> c -> IO (b, d)
once m f = runKleisli (reifyC (meterAction m (Kleisli (evaluate . force . f . hold))))
{-# INLINEABLE once #-}

-- | Measure a single call to a Kleisli arrow.
onceK :: (MonadFix m) => Meter (Kleisli m) a b -> Kleisli m c d -> c -> m (b, d)
onceK m k = runKleisli (reifyC (meterAction m k))
{-# INLINEABLE onceK #-}

-- | Reify a circuit using the cartesian tensor @(,)@.
--
-- Convenience alias for 'run' with @t = (,)@, used by the runners
-- below to extract a 'Kleisli' from a metered circuit.
--
-- 'Discrete' is required because free 'Trace' folds discharge 'Ob' via
-- 'withOb' (kind-gen / 'Category'+'Ob' hierarchy).
reifyC :: (Category arr, Traced (,) arr, Discrete arr) => Trace (,) arr a b -> arr a b
reifyC = run
{-# INLINEABLE reifyC #-}

-- | Single timing of a pure function. Returns @(nanos, result)@.
tick :: (NFData b) => (a -> b) -> a -> IO (Nanos, b)
tick = once timeX
{-# INLINEABLE tick #-}

-- ---------------------------------------------------------------------------
-- Repeated timing
-- ---------------------------------------------------------------------------

-- | @n@ timings of the same function. Returns @( [nanos], lastResult )@.
ticks :: (NFData b) => Int -> (a -> b) -> a -> IO ([Nanos], b)
ticks n f = runKleisli (timesC n timeX f)
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
-- IO repeated timing
-- ---------------------------------------------------------------------------

-- | @n@ timings of an 'IO' action with a default 100-iteration warmup.
-- Returns @( [nanos], lastResult )@.
ticksIO :: Int -> IO a -> IO ([Nanos], a)
ticksIO = ticksIOWithWarmup 100
{-# INLINEABLE ticksIO #-}

-- | @n@ timings of an 'IO' action with explicit warmup count.
ticksIOWithWarmup :: Int -> Int -> IO a -> IO ([Nanos], a)
ticksIOWithWarmup w n action = do
  replicateM_ (max 0 w) action
  let go 1 acc = do
        t0 <- nanos
        !a <- action
        t1 <- nanos
        pure (reverse ((t1 - t0) : acc), a)
      go i acc = do
        t0 <- nanos
        !_ <- action
        t1 <- nanos
        go (i - 1) ((t1 - t0) : acc)
  go (max 1 n) []
{-# INLINEABLE ticksIOWithWarmup #-}

-- | @n@ 'IO' timings collapsed to a single average nanosecond count.
ticksION :: Int -> IO a -> IO (Nanos, a)
ticksION n action = do
  (ts, a) <- ticksIO n action
  pure (sum ts `div` fromIntegral n, a)
{-# INLINEABLE ticksION #-}

-- ---------------------------------------------------------------------------
-- Plugin metering
-- ---------------------------------------------------------------------------

-- | Meter an 'IO' action with 'timeX'.
--
-- The result is a 'Circuit' polymorphic in the tensor @t@, so it can
-- be lifted into pipelines using either @(,)@ (lazy knot-tying) or
-- 'Either' (iteration) without changing the combinator.
meterIO :: (a -> IO b) -> Trace t (Kleisli IO) a (Nanos, b)
meterIO f = meterAction timeX (Kleisli f)
{-# INLINEABLE meterIO #-}

-- | Meter a pure function with 'timeX'. Forces to NF inside the timed
-- bracket so the work cannot be floated out.
meter :: (NFData b) => (a -> b) -> Trace t (Kleisli IO) a (Nanos, b)
meter f = meterAction timeX (Kleisli (evaluate . force . f . hold))
{-# INLINEABLE meter #-}

-- ---------------------------------------------------------------------------
-- Warmup
-- ---------------------------------------------------------------------------

-- | Warm up the clock with @n@ dummy reads. Avoids cold-start artefacts.
warmup :: Int -> IO ()
warmup n = replicateM_ n (void nanos)
{-# INLINE warmup #-}

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
-- >>> runKleisli (timesK 100 3 m (Kleisli (pure . (*2)))) 5
-- ([...,...,...],10)
timesK :: Int -> Int -> Meter (Kleisli IO) a b -> Kleisli IO c d -> Kleisli IO c ([b], d)
timesK w n m k = Kleisli \a -> do
  warmup w
  let step !x = runKleisli (reifyC (meterAction m k)) x
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
timesC :: (NFData d) => Int -> Meter (Kleisli IO) a b -> (c -> d) -> Kleisli IO c ([b], d)
timesC n m f = timesK 100 n m (Kleisli (evaluate . force . f . hold))
{-# INLINEABLE timesC #-}
