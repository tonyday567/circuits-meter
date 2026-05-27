{-# LANGUAGE BlockArguments #-}

-- | Performance measurement reimagined as a Circuit.
--
-- A 'Meter' @arr a b@ is a pair of arrows: 'initM' produces the initial
-- state, 'termM' observes it and produces a measurement. The bracketing
-- wires 'enterM' and 'exitM' are derived via the 'Strong' profunctor laws
-- — 'second'' with a swap for entry, 'first'' for exit.
--
-- The base arrow @arr@ is a type parameter: 'IO' only appears when you
-- instantiate @arr@ to 'Kleisli' 'IO'.  This makes 'Meter' reusable for
-- any 'Strong' arrow.
--
-- Meters compose sequentially with '≫' (chain the terminators) and in
-- parallel with 'both' (pair the states).
module Circuit.Meter
  ( -- * Meter
    Meter (..),
    MeterM,
    mkMeter,
    enterM,
    exitM,
    enterK,
    exitK,
    enter,
    exit,
    withMeter,
    meterC,
    meterA,
    meterK,
    (◅),
    (▻),
    both,

    -- * Sequential composition
    (≫),

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
import Control.Monad.Fix (MonadFix)
import Data.Profunctor
import Data.Tuple (swap)
import Prelude hiding (id, (.))

-- ---------------------------------------------------------------------------
-- Meter
-- ---------------------------------------------------------------------------

-- | A 'Meter' @arr a b@ is a pair of arrows.
--
-- * 'initM' — @() → arr a@ — produce the initial state.
-- * 'termM' — @a → arr b@ — observe the state and produce a measurement.
--
-- The bracketing wires are derived via 'Strong' profunctor laws:
--
-- @
--   enterM = dimap (,()) swap . second' . initM
--   exitM  = first' . termM
-- @
--
-- Sequential composition chains the terminators; parallel composition
-- pairs the states.  See '≫' and 'both'.
data Meter arr a b = Meter
  { initM :: arr () a,
    termM :: arr a b
  }

instance NFData (Meter arr a b) where
  rnf Meter{} = ()

-- | Convenience alias for 'Meter' over 'Kleisli'.
type MeterM m a b = Meter (Kleisli m) a b

-- | Construct a 'Meter' from raw monadic actions.
mkMeter :: m a -> (a -> m b) -> MeterM m a b
mkMeter pre post = Meter (Kleisli (const pre)) (Kleisli post)
{-# INLINEABLE mkMeter #-}

-- | Introduce the meter's state wire alongside the payload.
--
-- Derived from 'initM' via the 'Strong' profunctor law.
enterM :: (Strong arr) => Meter arr a b -> arr c (a, c)
enterM m = dimap (,()) swap (second' (initM m))
{-# INLINEABLE enterM #-}

-- | 'enterM' specialized to 'Kleisli'.
enterK :: (Monad m) => MeterM m a b -> Kleisli m c (a, c)
enterK = enterM
{-# INLINE enterK #-}

-- | Observe the state wire and pair the measurement with the result.
--
-- Derived from 'termM' via 'first''.
exitM :: (Strong arr) => Meter arr a b -> arr (a, c) (b, c)
exitM = first' . termM
{-# INLINEABLE exitM #-}

-- | 'exitM' specialized to 'Kleisli'.
exitK :: (Monad m) => MeterM m a b -> Kleisli m (a, c) (b, c)
exitK = exitM
{-# INLINE exitK #-}

-- | Lift 'enterM' into a 'Circuit'.
--
-- Tensor-agnostic: the implementation is just 'Lift', so @t@ can be
-- any tensor. The value-level pairing @(a, c)@ comes from 'Strong'.
enter :: (Strong arr) => Meter arr a b -> Circuit arr t c (a, c)
enter = Lift . enterM
{-# INLINEABLE enter #-}

-- | Lift 'exitM' into a 'Circuit'.
--
-- Tensor-agnostic: the implementation is just 'Lift', so @t@ can be
-- any tensor. The value-level pairing @(a, c)@ comes from 'Strong'.
exit :: (Strong arr) => Meter arr a b -> Circuit arr t (a, c) (b, c)
exit = Lift . exitM
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
c ▻ m = Lift (dimap id snd (exitM m)) . c
{-# INLINE (▻) #-}

-- | Apply a 'Meter' to a 'Circuit', keeping the measurement.
meterC :: (Category arr, Strong arr, Trace arr (,)) => Meter arr a b -> Circuit arr (,) c d -> Circuit arr (,) c (b, d)
meterC m c = withMeter m (ambient c)
{-# INLINEABLE meterC #-}

-- | Apply a 'Meter' to an arrow, keeping the measurement.
--
-- @
--   meterA m k = reify (meterC m (Lift k))
-- @
meterA :: (Category arr, Strong arr, Trace arr (,)) => Meter arr a b -> arr c d -> arr c (b, d)
meterA m k = reify (meterC m (Lift k))
{-# INLINEABLE meterA #-}

-- | 'meterA' specialized to 'Kleisli'.
meterK :: (MonadFix m) => MeterM m a b -> Kleisli m c d -> Kleisli m c (b, d)
meterK = meterA
{-# INLINEABLE meterK #-}

-- | Sequential composition of meters.
--
-- The 'initM' of the result is the first meter's 'initM';
-- the 'termM' chains both terminators.
--
-- @
--   postprocess ≫ timeM
-- @
infixr 7 ≫

(≫) :: (Category arr) => Meter arr a b -> Meter arr b c -> Meter arr a c
m1 ≫ m2 = Meter (initM m1) (termM m2 . termM m1)
{-# INLINE (≫) #-}

-- | Run two meters simultaneously.
--
-- The state wires are independent; the @(,)@ tensor handles the
-- wiring automatically. 'both' is the product of meters.
both :: (Arrow arr) => Meter arr a1 b1 -> Meter arr a2 b2 -> Meter arr (a1, a2) (b1, b2)
both m1 m2 =
  Meter
    { initM = initM m1 &&& initM m2,
      termM = termM m1 *** termM m2
    }
{-# INLINEABLE both #-}

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
meterPure :: (MonadFix m, NFData d) => MeterM m a b -> (c -> d) -> Circuit (Kleisli m) (,) c (b, d)
meterPure m f = meterAction m (Kleisli (pure . force . f . hold))
{-# INLINEABLE meterPure #-}

-- | Hold back a value so GHC cannot float a function application past
-- the meter boundary. Re-exported for custom meter authors.
hold :: a -> a
hold x = x
{-# NOINLINE hold #-}
