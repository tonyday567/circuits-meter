{-# LANGUAGE BlockArguments #-}

-- | Time measurement as a Circuit.
--
-- 'timeM' is the canonical 'Meter' for nanosecond timing. All other
-- time combinators are derived from it via 'Circuit.Perf.meterK' and
-- the 'Trace' iteration structure.
module Circuit.Perf.Time
  ( -- * Time meter
    timeM,

    -- * Single timing
    tick,
    tickForce,

    -- * Repeated timing
    ticks,
    ticksN,

    -- * StepMeasure compatibility
    stepTime,
  )
where

import Circuit.Perf
import Control.Arrow
import Control.DeepSeq
import Prelude hiding (id, (.))

-- | Read clock before and after; return the delta in nanoseconds.
--
-- >>> runKleisli (meterK timeM (Kleisli (pure . (*2)))) 5
-- (10, ...nanos...)
timeM :: Meter Nanos Nanos
timeM =
  Meter
    { pre = nanos,
      post = \s -> do
        !e <- nanos
        pure (e - s)
    }
{-# INLINEABLE timeM #-}

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

-- | @n@ timings of the same function. Returns @( [nanos], lastResult )@.
ticks :: (NFData b) => Int -> (a -> b) -> a -> IO ([Nanos], b)
ticks n f a = runKleisli (timesC n timeM f) a
{-# INLINEABLE ticks #-}

-- | @n@ timings collapsed to a single average nanosecond count.
--
-- The computation is run @n@ times; the total time is divided by @n@.
ticksN :: (NFData b) => Int -> (a -> b) -> a -> IO (Nanos, b)
ticksN n f a = do
  (ts, b) <- ticks n f a
  pure (sum ts `div` fromIntegral n, b)
{-# INLINEABLE ticksN #-}

-- | 'StepMeasure'-style clock read for interoperability with the
-- @perf@ library pattern.
stepTime :: Meter Nanos Nanos
stepTime = timeM
{-# INLINE stepTime #-}
