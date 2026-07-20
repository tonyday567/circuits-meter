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

    -- * Meter composition
    both,

    -- * Plugin metering
    meterAction,

    -- * Hold
    hold,
  )
where

import Circuit.Loop (Loop (..))
import Control.Arrow
import Control.Category
import Control.DeepSeq
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
meterAction :: (Category arr, Strong arr) => Meter arr a b -> arr c d -> Loop t arr c (b, d)
meterAction m k =
  Lift (first' (stop m) . second' k . dimap ((),) id (first' (start m)))
{-# INLINEABLE meterAction #-}

-- | Hold back a value so GHC cannot float a function application past
-- the meter boundary. Re-exported for custom meter authors.
hold :: a -> a
hold x = x
{-# NOINLINE hold #-}
