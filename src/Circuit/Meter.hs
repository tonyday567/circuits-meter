{-# LANGUAGE BlockArguments #-}

-- | Performance measurement reimagined as a Circuit.
--
-- A 'Meter' @arr a b@ is a pair of arrows: 'start' produces the initial
-- state, 'stop' observes it and produces a measurement. The canonical
-- use is 'meterAction', which sandwiches a payload arrow between the
-- two meter arrows.
module Circuit.Meter
  ( -- * Meter
    Meter (..),
    mkMeter,

    -- * Cartesian helpers for Kleisli arrows
    firstK,
    secondK,
    dimapK,

    -- * Meter composition
    both,

    -- * Plugin metering
    meterAction,

    -- * Hold
    hold,
  )
where

import Circuit.Category (Category (..), K (..))
import Circuit.Trace (Trace, base)
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
data Meter arr a b = Meter
  { start :: arr () a,
    stop :: arr a b
  }

-- | Construct a 'Meter' from raw monadic actions.
mkMeter :: m a -> (a -> m b) -> Meter (K m) a b
mkMeter pre post = Meter (K (const pre)) (K post)
{-# INLINEABLE mkMeter #-}

-- ---------------------------------------------------------------------------
-- Cartesian helpers for Kleisli arrows
-- ---------------------------------------------------------------------------

-- | First component for @K m@.
firstK :: (Functor m) => K m a b -> K m (a, c) (b, c)
firstK (K k) = K (\(a, c) -> fmap (\b -> (b, c)) (k a))
{-# INLINEABLE firstK #-}

-- | Second component for @K m@.
secondK :: (Functor m) => K m a b -> K m (c, a) (c, b)
secondK (K k) = K (\(c, a) -> fmap (\b -> (c, b)) (k a))
{-# INLINEABLE secondK #-}

-- | Profunctor-style pre/post composition for @K m@.
dimapK :: (Functor m) => (a' -> a) -> (b -> b') -> K m a b -> K m a' b'
dimapK f g (K k) = K (fmap g . k . f)
{-# INLINEABLE dimapK #-}

-- | Run two meters simultaneously.
--
-- The state wires are independent; the @(,)@ tensor handles the
-- wiring automatically.
both :: (Monad m) => Meter (K m) a1 b1 -> Meter (K m) a2 b2 -> Meter (K m) (a1, a2) (b1, b2)
both m1 m2 =
  Meter
    { start = dimapK (\() -> ((), ())) id (firstK (start m1) . secondK (start m2)),
      stop = firstK (stop m1) . secondK (stop m2)
    }
{-# INLINEABLE both #-}

-- ---------------------------------------------------------------------------
-- Plugin metering
-- ---------------------------------------------------------------------------

-- | Meter a Kleisli arrow action, keeping the measurement.
--
-- Tensor-agnostic: the bracket is built directly in the base arrow
-- and lifted with 'base', so the meter state is introduced and consumed
-- locally. The result is a 'Circuit' polymorphic in the tensor @t@.
--
-- For arrow-level extraction, use 'eval' with your chosen tensor.
meterAction :: (Monad m) => Meter (K m) a b -> K m c d -> Trace t (K m) c (b, d)
meterAction m k =
  base (firstK (stop m) . secondK k . dimapK ((),) id (firstK (start m)))
{-# INLINEABLE meterAction #-}

-- | Hold back a value so GHC cannot float a function application past
-- the meter boundary. Re-exported for custom meter authors.
hold :: a -> a
hold x = x
{-# NOINLINE hold #-}
