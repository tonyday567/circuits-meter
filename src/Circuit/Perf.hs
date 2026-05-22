{-# LANGUAGE BlockArguments #-}

-- | Performance measurement reimagined as a Circuit.
--
-- A 'Meter' is the circuit analogue of 'StepMeasure': it introduces a
-- state wire before a computation and reads it after. The state wire is
-- the feedback channel — measurement is the act of exposing it.
--
-- @
--   pre  :: IO s        -- introduce the state wire
--   f    :: a -> IO b   -- the computation
--   post :: s -> IO t   -- observe the wire
-- @
--
-- Composed as a Kleisli circuit:
--
-- @
--   meterK (Meter pre post) (Kleisli f)
--     = Kleisli \a -> do s <- pre; b <- f a; t <- post s; pure (t, b)
-- @
--
-- Bracket syntax for metering a section:
--
-- @
--   timeM ◅ two ↣ three ▻ timeM
-- @
module Circuit.Perf
  ( -- * Clock
    Nanos,
    nanos,

    -- * Meter
    Meter (..),
    preC,
    postC,
    postC_,
    meterK,
    meterK_,
    meterC,
    meterC_,
    (↣),
    (◅),
    (▻),
    both,

    -- * Single measurement
    once,
    once_,

    -- * Repeated measurement
    timesK,
    timesC,
    times_,

    -- * Warmup
    warmup,
    warmupK,
  )
where

import Circuit.Braided (ambient)
import Circuit.Circuit
import Circuit.Traced (Trace)
import Control.Arrow
import Control.Category
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Profunctor (Profunctor)
import System.Clock
import Prelude hiding (id, (.))

-- | Local synonym for 'Compose'.
infixr 9 ⊙

(⊙) :: Circuit arr t b c -> Circuit arr t a b -> Circuit arr t a c
(⊙) = Compose
{-# INLINE (⊙) #-}

-- | Left-to-right sequential composition.
infixl 9 ↣

(↣) :: Circuit arr t a b -> Circuit arr t b c -> Circuit arr t a c
f ↣ g = g ⊙ f
{-# INLINE (↣) #-}

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
-- Meter
-- ---------------------------------------------------------------------------

-- | A 'Meter' introduces a state wire, runs a computation, and observes
-- the wire afterward. Both the state type @s@ and the measurement type
-- @t@ are visible, which lets you bracket a circuit with meter symbols
-- on either end.
--
-- This is the circuit analogue of 'StepMeasure' from the @perf@ library.
data Meter s t = Meter
  { pre :: IO s,
    post :: s -> IO t
  }

-- | Introduce a state wire. Lifts a 'pre' action into the @(,)@ tensor.
preC :: IO s -> Circuit (Kleisli IO) (,) a (s, a)
preC pre = Lift (Kleisli \a -> (,) <$> pre <*> pure a)
{-# INLINEABLE preC #-}

-- | Observe a state wire. Lifts a 'post' action out of the @(,)@ tensor.
postC :: (s -> IO t) -> Circuit (Kleisli IO) (,) (s, b) (t, b)
postC post = Lift (Kleisli \(s, b) -> (,) <$> post s <*> pure b)
{-# INLINEABLE postC #-}

-- | Observe a state wire and discard the measurement.
-- The pipeline continues with the payload unchanged.
postC_ :: (s -> IO t) -> Circuit (Kleisli IO) (,) (s, b) b
postC_ post = Lift (Kleisli \(s, b) -> b <$ post s)
{-# INLINEABLE postC_ #-}

-- | 'ambient' already threads a state wire through a circuit using the
-- canonical braid from the 'Braided' instance.  This alias keeps the
-- old name for backward compatibility.
ambientPair :: (Profunctor arr, Trace arr (,)) => Circuit arr (,) a b -> Circuit arr (,) (s, a) (s, b)
ambientPair = ambient
{-# INLINEABLE ambientPair #-}

-- | Left meter bracket. Introduces the meter's state wire and threads
-- it through the circuit to the right.
--
-- @
--   timeM ◅ two
-- @
infixl 5 ◅

(◅) :: Meter s t -> Circuit (Kleisli IO) (,) a b -> Circuit (Kleisli IO) (,) a (s, b)
m ◅ c = ambientPair c ⊙ preC (pre m)
{-# INLINE (◅) #-}

-- | Right meter bracket (discard). Observes the state wire and drops
-- the measurement, restoring the original payload type.
--
-- @
--   (timeM ◅ two) ▻ timeM
-- @
infixl 5 ▻

(▻) :: Circuit (Kleisli IO) (,) a (s, b) -> Meter s t -> Circuit (Kleisli IO) (,) a b
c ▻ m = postC_ (post m) ⊙ c
{-# INLINE (▻) #-}

-- | Apply a 'Meter' to a 'Circuit', keeping the measurement.
meterC :: Meter s t -> Circuit (Kleisli IO) (,) a b -> Circuit (Kleisli IO) (,) a (t, b)
meterC m c = postC (post m) ⊙ (m ◅ c)
{-# INLINEABLE meterC #-}

-- | Apply a 'Meter' to a 'Circuit', discarding the measurement.
-- The circuit's interface is unchanged.
--
-- @
--   meterC_ m c = (m ◅ c) ▻ m
-- @
meterC_ :: Meter s t -> Circuit (Kleisli IO) (,) a b -> Circuit (Kleisli IO) (,) a b
meterC_ m c = (m ◅ c) ▻ m
{-# INLINEABLE meterC_ #-}

-- | Apply a 'Meter' to a 'Kleisli' arrow, keeping the measurement.
--
-- @
--   meterK m k = reify (meterC m (Lift k))
-- @
meterK :: Meter s t -> Kleisli IO a b -> Kleisli IO a (t, b)
meterK m k = reify (meterC m (Lift k))
{-# INLINEABLE meterK #-}

-- | Apply a 'Meter' to a 'Kleisli' arrow, discarding the measurement.
meterK_ :: Meter s t -> Kleisli IO a b -> Kleisli IO a b
meterK_ m k = reify (meterC_ m (Lift k))
{-# INLINEABLE meterK_ #-}

-- | Run two meters simultaneously.
--
-- The state wires are independent; the @(,)@ tensor handles the
-- wiring automatically. 'both' is the product of meters.
both :: Meter s1 t1 -> Meter s2 t2 -> Meter (s1, s2) (t1, t2)
both (Meter p1 p2) (Meter q1 q2) =
  Meter
    { pre = (,) <$> p1 <*> q1,
      post = \(s1, s2) -> (,) <$> p2 s1 <*> q2 s2
    }
{-# INLINEABLE both #-}

-- ---------------------------------------------------------------------------
-- Single measurement
-- ---------------------------------------------------------------------------

-- | Hold back a value so GHC cannot float a function application past
-- the meter boundary. Used internally by 'once' and 'timesC'.
hold :: a -> a
hold x = x
{-# NOINLINE hold #-}

-- | Measure a single call to a pure function. Forces the result to NF
-- inside the timed IO action so the work cannot be floated out.
once :: (NFData b) => Meter s t -> (a -> b) -> a -> IO (t, b)
once m f = runKleisli (meterK m (Kleisli (evaluate . force . f . hold)))
{-# INLINEABLE once #-}

-- | Measure a single call, discarding the result.
once_ :: (NFData b) => Meter s t -> (a -> b) -> a -> IO t
once_ m f a = fst <$> once m f a
{-# INLINEABLE once_ #-}

-- ---------------------------------------------------------------------------
-- Repeated measurement
-- ---------------------------------------------------------------------------

-- | Measure a 'Kleisli' arrow repeated @n@ times. Returns per-run
-- measurements and the last result.
--
-- The step is marked 'NOINLINE' so GHC cannot float the computation
-- out of the timing loop.
--
-- >>> let m = Meter nanos (\s -> do e <- nanos; pure (e - s))
-- >>> runKleisli (timesK 3 m (Kleisli (pure . (*2)))) 5
-- ([..., ..., ...], 10)
timesK :: Int -> Meter s t -> Kleisli IO a b -> Kleisli IO a ([t], b)
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
-- to NF inside the timed IO action so the work cannot be floated out.
timesC :: (NFData b) => Int -> Meter s t -> (a -> b) -> Kleisli IO a ([t], b)
timesC n m f = timesK n m (Kleisli (evaluate . force . f . hold))
{-# INLINEABLE timesC #-}

-- | Repeated measurement, discarding results.
times_ :: (NFData b) => Int -> Meter s t -> (a -> b) -> a -> IO [t]
times_ n m f a = fst <$> runKleisli (timesC n m f) a
{-# INLINEABLE times_ #-}

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
