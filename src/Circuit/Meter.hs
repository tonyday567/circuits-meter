{-# LANGUAGE BlockArguments #-}

-- | Performance measurement reimagined as a Circuit.
--
-- A 'Meter' @arr a b@ is a pair of arrows: 'start' produces the initial
-- state, 'stop' observes it and produces a measurement. The bracketing
-- wires 'enter' and 'exit' are derived via the 'Strong' profunctor laws
-- — 'second'' with a swap for entry, 'first'' for exit.
--
-- The base arrow @arr@ is a type parameter: 'IO' only appears when you
-- instantiate @arr@ to 'Kleisli' 'IO'.  This makes 'Meter' reusable for
-- any 'Strong' arrow.
module Circuit.Meter
  ( -- * Meter
    Meter (..),
    mkMeter,

    -- * Meter composition
    (≫),
    both,
    enter,
    exit,
    withMeter,
    (◅),
    (▻),

    -- * Plugin metering
    meterAction,
    meterPure,

    -- * Hold
    hold,
  )
where

import Circuit.Monoidal (ambient)
import Circuit.Trace
import Circuit.Trace (Traced)
import Control.Arrow
import Control.Category
import Control.DeepSeq
-- import Control.Monad.Fix (MonadFix)
import Data.Profunctor
import Prelude hiding (id, (.))

-- ---------------------------------------------------------------------------
-- Meter
-- ---------------------------------------------------------------------------

-- | A 'Meter' @arr a b@ is a stopwatch: a pair of arrows.
--
-- * 'start' — @() → arr a@ — capture initial state (e.g. read the clock).
-- * 'stop' — @a → arr b@ — observe the state and produce a measurement
--   (e.g. read the clock again and subtract).  May be called multiple
--   times; each call measures elapsed time since the single 'start'.
--
-- The bracketing wires are derived via 'Strong' profunctor laws:
--
-- @
--   enter = Arr (dimap (,()) swap (second' (start m)))
--   exit  = Arr (first' (stop m))
-- @
--
-- Sequential composition chains the terminators; parallel composition
-- pairs the states.
data Meter arr a b = Meter
  { start :: arr () a,
    stop :: arr a b
  }

instance NFData (Meter arr a b) where
  rnf Meter {} = ()

-- | Construct a 'Meter' from raw monadic actions.
mkMeter :: m a -> (a -> m b) -> Meter (Kleisli m) a b
mkMeter pre post = Meter (Kleisli (const pre)) (Kleisli post)
{-# INLINEABLE mkMeter #-}

-- These are not lawful instances: 'Category' needs an identity with
-- @start :: arr () a@ for all @a@ (impossible — start is effectful);
-- 'Arrow' needs @arr :: (b -> c) -> Meter arr b c@ (impossible — no
-- pure start).  They are provided as plain combinators instead.

-- | Sequential composition of meters.
--
-- The 'start' of the result is the first meter's 'start';
-- the 'stop' chains both terminators.
infixr 7 ≫

(≫) :: (Category arr) => Meter arr a b -> Meter arr b c -> Meter arr a c
m1 ≫ m2 = Meter (start m1) (stop m2 . stop m1)
{-# INLINE (≫) #-}

-- | Run two meters simultaneously.
--
-- The state wires are independent; the @(,)@ tensor handles the
-- wiring automatically.
both :: (Arrow arr) => Meter arr a1 b1 -> Meter arr a2 b2 -> Meter arr (a1, a2) (b1, b2)
both m1 m2 =
  Meter
    { start = start m1 &&& start m2,
      stop = stop m1 *** stop m2
    }
{-# INLINEABLE both #-}

-- | Introduce the meter's state wire alongside the payload.
--
-- Derived from 'start' via the 'Strong' profunctor law.  Tensor-agnostic:
-- the implementation is just 'Lift', so @t@ can be any tensor.  The
-- value-level pairing @(a, c)@ comes from 'Strong'.
enter :: (Strong arr) => Meter arr a b -> Trace t arr c (a, c)
enter m = Arr (dimap ((),) id (first' (start m)))
{-# INLINEABLE enter #-}

-- | Observe the state wire and pair the measurement with the result.
--
-- Derived from 'stop' via 'first''.  Tensor-agnostic: the
-- implementation is just 'Arr'.
exit :: (Strong arr) => Meter arr a b -> Trace t arr (a, c) (b, c)
exit = Arr . first' . stop
{-# INLINEABLE exit #-}

-- | Bracket an inner circuit with a meter.  Pure composition.
--
-- Tensor-agnostic: the implementation is sequential composition in
-- 'Trace', so @t@ can be any tensor. The value-level pairing @(a, c)@
-- comes from 'Strong' via 'enter' and 'exit'.
withMeter :: (Category arr, Strong arr, Traced t arr) => Meter arr a b -> Trace t arr (a, c) (a, d) -> Trace t arr c (b, d)
withMeter m inner = exit m . inner . enter m
{-# INLINEABLE withMeter #-}

-- | Left meter bracket. Introduces the meter's state wire and threads
-- it through the circuit to the right.
--
-- @
--   timeM ◅ two
-- @
infixl 5 ◅

(◅) :: (Category arr, Strong arr, Traced (,) arr) => Meter arr a b -> Trace (,) arr c d -> Trace (,) arr c (a, d)
m ◅ c = ambient c . enter m
{-# INLINE (◅) #-}

-- | Right meter bracket (discard). Observes the state wire and drops
-- the measurement, restoring the original payload type.
--
-- @
--   (timeM ◅ two) ▻ timeM
-- @
infixl 5 ▻

(▻) :: (Category arr, Strong arr, Traced t arr) => Trace t arr c (a, d) -> Meter arr a b -> Trace t arr c d
c ▻ m = Arr (dimap id snd (first' (stop m))) . c
{-# INLINE (▻) #-}

-- ---------------------------------------------------------------------------
-- Plugin metering
-- ---------------------------------------------------------------------------

-- | Meter an arrow action, keeping the measurement.
--
-- Tensor-agnostic: the bracket is built directly in the base arrow
-- and lifted with 'Arr', so it only needs 'Category' + 'Strong'.
-- The meter state is introduced and consumed locally; the result is
-- a 'Circuit' polymorphic in the tensor @t@.
--
-- For arrow-level extraction, use 'run' with your chosen tensor.
meterAction :: (Category arr, Strong arr) => Meter arr a b -> arr c d -> Trace t arr c (b, d)
meterAction m k =
  Arr (first' (stop m) . second' k . dimap ((),) id (first' (start m)))
{-# INLINEABLE meterAction #-}

-- | Meter a pure function.  Forces to NF inside the bracket.
--
-- __Note__: This uses @pure . force@, not 'evaluate', so GHC may float
-- the work outside the meter bracket.  For 'IO'-based benchmarking use
-- 'Circuit.Meter.Time.meter' instead, which forces inside 'IO'.
--
-- >>> import Circuit (Trace, run)
-- >>> import Control.Arrow (Kleisli(..), runKleisli)
-- >>> import Data.Functor.Identity
-- >>> let m = Meter (Kleisli (\_ -> Identity (0::Int))) (Kleisli (\_ -> Identity (1::Int)))
-- >>> runKleisli (run (meterPure m (+1) :: Trace (,) (Kleisli Identity) Int (Int, Int))) (5::Int)
-- Identity (1,6)
meterPure :: (Monad m, NFData d) => Meter (Kleisli m) a b -> (c -> d) -> Trace t (Kleisli m) c (b, d)
meterPure m f = meterAction m (Kleisli (pure . force . f . hold))
{-# INLINEABLE meterPure #-}

-- | Hold back a value so GHC cannot float a function application past
-- the meter boundary. Re-exported for custom meter authors.
hold :: a -> a
hold x = x
{-# NOINLINE hold #-}
