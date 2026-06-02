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
--
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
    meterC,
    meterA,
    meterArr,
    (◅),
    (▻),
    -- * Plugin metering
    meterAction,
    meterPure,

    -- * Hold
    hold,
  )
where

import Circuit.Circuit
import Circuit.Monoidal (ambient)
import Circuit.Traced (Trace)
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
--   enter = Lift (dimap (,()) swap (second' (start m)))
--   exit  = Lift (first' (stop m))
-- @
--
-- Sequential composition chains the terminators; parallel composition
-- pairs the states.
data Meter arr a b = Meter
  { start :: arr () a,
    stop :: arr a b
  }

instance NFData (Meter arr a b) where
  rnf Meter{} = ()

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
enter :: (Strong arr) => Meter arr a b -> Circuit arr t c (a, c)
enter m = Lift (dimap ((),) id (first' (start m)))
{-# INLINEABLE enter #-}

-- | Observe the state wire and pair the measurement with the result.
--
-- Derived from 'stop' via 'first''.  Tensor-agnostic: the
-- implementation is just 'Lift'.
exit :: (Strong arr) => Meter arr a b -> Circuit arr t (a, c) (b, c)
exit = Lift . first' . stop
{-# INLINEABLE exit #-}

-- | Bracket an inner circuit with a meter.  Pure composition.
--
-- Tensor-agnostic: the implementation is just sequential 'Compose',
-- so @t@ can be any tensor. The value-level pairing @(a, c)@ comes
-- from 'Strong' via 'enter' and 'exit'.
withMeter :: (Category arr, Strong arr) => Meter arr a b -> Circuit arr t (a, c) (a, d) -> Circuit arr t c (b, d)
withMeter m inner = exit m . inner . enter m
{-# INLINEABLE withMeter #-}

-- | Left meter bracket. Introduces the meter's state wire and threads
-- it through the circuit to the right.
--
-- @
--   timeM ◅ two
-- @
infixl 5 ◅

(◅) :: (Category arr, Strong arr, Trace arr (,)) => Meter arr a b -> Circuit arr (,) c d -> Circuit arr (,) c (a, d)
m ◅ c = ambient c . enter m
{-# INLINE (◅) #-}

-- | Right meter bracket (discard). Observes the state wire and drops
-- the measurement, restoring the original payload type.
--
-- @
--   (timeM ◅ two) ▻ timeM
-- @
infixl 5 ▻

(▻) :: (Category arr, Strong arr) => Circuit arr t c (a, d) -> Meter arr a b -> Circuit arr t c d
c ▻ m = Lift (dimap id snd (first' (stop m))) . c
{-# INLINE (▻) #-}

-- | Apply a 'Meter' to a 'Circuit', keeping the measurement.
meterC :: (Category arr, Strong arr, Trace arr (,)) => Meter arr a b -> Circuit arr (,) c d -> Circuit arr (,) c (b, d)
meterC m c = withMeter m (ambient c)
{-# INLINEABLE meterC #-}

-- | Apply a 'Meter' to an arrow, keeping the measurement.
--
-- Only requires 'Arrow', not 'Trace', making it usable with any tensor
-- once the result is lifted into a 'Circuit'.
--
-- @
--   meterA m k = (arr (const ()) >>> start m) &&& k >>> first (stop m)
-- @
meterA :: (Arrow arr) => Meter arr a b -> arr c d -> arr c (b, d)
meterA = meterArr
{-# INLINEABLE meterA #-}

-- | Low-level arrow metering.  Brackets a single arrow with a meter's
-- 'start' and 'stop', pairing the measurement with the result.
--
-- This is the primitive behind 'meterA': it needs only 'Arrow',
-- whereas 'meterC' / 'meterAction' need 'Trace arr (,)' because they
-- thread the meter state through an entire 'Circuit' via 'ambient'.
meterArr :: (Arrow arr) => Meter arr a b -> arr c d -> arr c (b, d)
meterArr m k = (arr (const ()) >>> start m) &&& k >>> first (stop m)
{-# INLINEABLE meterArr #-}

-- ---------------------------------------------------------------------------
-- Plugin metering
-- ---------------------------------------------------------------------------

-- | Meter a monadic action.  Plugin-level combinator.
meterAction :: (Category arr, Strong arr, Trace arr (,)) => Meter arr a b -> arr c d -> Circuit arr (,) c (b, d)
meterAction m = withMeter m . ambient . Lift
{-# INLINEABLE meterAction #-}

-- | Meter a pure function.  Forces to NF inside the bracket.
--
-- __Note__: This uses @pure . force@, not 'evaluate', so GHC may float
-- the work outside the meter bracket.  For 'IO'-based benchmarking use
-- 'Circuit.Meter.Time.meter' instead, which forces inside 'IO'.
--
-- >>> import Circuit (reify)
-- >>> import Control.Arrow (Kleisli(..), runKleisli)
-- >>> import Data.Functor.Identity
-- >>> let m = Meter (Kleisli (\_ -> Identity (0::Int))) (Kleisli (\_ -> Identity (1::Int)))
-- >>> runKleisli (reify (meterPure m (+1))) (5::Int)
-- Identity (1,6)
meterPure :: (Monad m, NFData d) => Meter (Kleisli m) a b -> (c -> d) -> Circuit (Kleisli m) t c (b, d)
meterPure m f = Lift (meterA m (Kleisli (pure . force . f . hold)))
{-# INLINEABLE meterPure #-}

-- | Hold back a value so GHC cannot float a function application past
-- the meter boundary. Re-exported for custom meter authors.
hold :: a -> a
hold x = x
{-# NOINLINE hold #-}
