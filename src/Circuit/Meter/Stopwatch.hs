{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

-- | Stopwatch/interval metering as a first-class circuit wire.
--
-- Instead of wrapping each stage individually, drop named markers into a
-- left-to-right pipeline. The timing log travels on a cartesian @(,)@ wire
-- alongside the payload.
--
-- A single watch is active at any point. 'start' sets the active watch;
-- 'lap' records an interval since the previous 'start' or 'lap' under a
-- given label and starts a fresh interval on the same active watch;
-- 'stop' records the final interval and keeps the log.
module Circuit.Meter.Stopwatch
  ( -- * Timing log
    Watches,
    allLaps,
    watchLaps,

    -- * Markers
    start,
    lap,
    stop,

    -- * Stage sugar
    carry,
    carryT,
    meterIt,
    timeIt,
    meterItN,
    timeItN,
  )
where

import Circuit
import Circuit.Meter (Meter)
import Circuit.Meter qualified as Meter
import Circuit.Meter.Time (Nanos, timeX)
import Control.Arrow (Kleisli (..), first)
import Control.Category ((.), (>>>))
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (replicateM_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Prelude hiding (id, (.))

-- ---------------------------------------------------------------------------
-- Timing log
-- ---------------------------------------------------------------------------

-- | Stopwatch state: the active watch name, the meter state for the current
-- in-flight interval, and the log of completed intervals.
data Watches x y = Watches
  { active :: String,
    startState :: x,
    laps :: Map String [y]
  }

-- | Extract all measurements for a named label, oldest first.
watchLaps :: Watches x y -> String -> [y]
watchLaps Watches {..} name = maybe [] reverse $ Map.lookup name laps

-- | Extract all measurements for all labels, oldest first.
allLaps :: Watches x y -> Map String [y]
allLaps Watches {..} = Map.map reverse laps

-- ---------------------------------------------------------------------------
-- Markers
-- ---------------------------------------------------------------------------

-- | Start a named watch. Sets the active watch and initializes the meter
-- state for the first interval.
start :: Meter (Kleisli IO) x y -> String -> Trace (,) (Kleisli IO) a (a, Watches x y)
start m name = Arr $ Kleisli $ \a -> do
  x <- runKleisli (Meter.start m) ()
  pure (a, Watches name x Map.empty)

-- | Record a lap: stop the current interval, store the measurement under
-- @label@, and start a fresh interval on the same active watch.
lap :: Meter (Kleisli IO) x y -> String -> Trace (,) (Kleisli IO) (a, Watches x y) (a, Watches x y)
lap m label = Arr $ Kleisli $ \(a, ws) -> do
  y <- runKleisli (Meter.stop m) (startState ws)
  x' <- runKleisli (Meter.start m) ()
  pure (a, ws {startState = x', laps = Map.insertWith (++) label [y] (laps ws)})

-- | Stop the active watch: record the final interval under @name@ and keep
-- the log.
stop :: Meter (Kleisli IO) x y -> String -> Trace (,) (Kleisli IO) (a, Watches x y) (a, Watches x y)
stop m name = Arr $ Kleisli $ \(a, ws) -> do
  y <- runKleisli (Meter.stop m) (startState ws)
  pure (a, ws {laps = Map.insertWith (++) name [y] (laps ws)})

-- ---------------------------------------------------------------------------
-- Stage sugar
-- ---------------------------------------------------------------------------

-- | Lift a base arrow so it carries the timing wire unchanged.
carry :: Kleisli IO a b -> Trace (,) (Kleisli IO) (a, Watches x y) (b, Watches x y)
carry stage = Arr (first stage)

-- | Lift an already-built 'Trace' stage so it carries the timing wire
-- unchanged. The stage is run at its own tensor and then threaded through the
-- cartesian timing wire.
carryT :: (Traced t (Kleisli IO)) => Trace t (Kleisli IO) a b -> Trace (,) (Kleisli IO) (a, Watches x y) (b, Watches x y)
carryT stage = Arr (first (run stage))

-- | Meter a single stage: start, run the stage, stop.
meterIt :: Meter (Kleisli IO) x y -> String -> Kleisli IO a b -> Trace (,) (Kleisli IO) a (b, Watches x y)
meterIt m name stage =
  start m name
    >>> carry stage
    >>> stop m name

-- | 'meterIt' with the default time meter.
timeIt :: String -> Kleisli IO a b -> Trace (,) (Kleisli IO) a (b, Watches Nanos Nanos)
timeIt = meterIt timeX

-- | Meter a stage over @n@ repetitions and record the total measurement.
--
-- The stage is run @n@ times between 'start' and 'stop'; the recorded lap is
-- the total time/space for all runs. Divide by @n@ for a per-iteration average.
-- The last result is kept and forced to NF; intermediate results are also forced
-- so the work cannot be floated out of the loop.
meterItN :: (NFData b) => Int -> Meter (Kleisli IO) x y -> String -> Kleisli IO a b -> Trace (,) (Kleisli IO) a (b, Watches x y)
meterItN n0 m name stage =
  start m name
    >>> carry (Kleisli loop)
    >>> stop m name
  where
    n = max 1 n0
    loop a = do
      b <- runKleisli stage a >>= evaluate . force
      replicateM_ (n - 1) $ do
        !_ <- runKleisli stage a >>= evaluate . force
        pure ()
      pure b

-- | 'meterItN' with the default time meter.
timeItN :: (NFData b) => Int -> String -> Kleisli IO a b -> Trace (,) (Kleisli IO) a (b, Watches Nanos Nanos)
timeItN n = meterItN n timeX
