{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

-- | Input-seeded meters, repeated runners, and labelled pipeline measurement.
module Circuit.Meter
  ( Meter,
    nShot,
    Nanos (..),
    Bytes (..),
    stamp,
    elapsed,
    time,
    timeIO,
    spaceIO,
    spaceGCIO,
    timeSpaceIO,
    timeSpaceGCIO,
    warmupClock,
    warmupPayload,
    Bracket (..),
    clock,
    allocation,
    allocationGC,
    timeSpace,
    timeSpaceGC,
    Pipeline,
    TimePipeline,
    laps,
    lapsFor,
    start,
    lap,
    stop,
    carry,
    carryT,
  )
where

import Circuit.Category (K (..))
import Circuit.Machine (Body (..))
import Circuit.Syntax (Syntax (Lift), eval)
import Circuit.Tensor (Const (..))
import Circuit.Trace (FreeTrace)
import Circuit.Traced (Trace)
import Control.Exception (evaluate)
import Control.Monad (replicateM_, void)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)
import GHC.Stats (allocated_bytes, getRTSStats)
import System.Clock
import System.Mem (performGC)
import Prelude hiding (init)

-- | A diagonal, input-seeded machine whose final observation is emitted from
-- the fresh channel.
type Meter m ch a o = Body (,) Const ch ch (K m) a o

-- | Initialise with the first input and commit the remaining n−1 identical
-- inputs directly, without constructing an input tape. Zero and negative
-- counts perform no init, commit, or emit and produce no observation.
nShot :: (Monad m) => Int -> Meter m ch a o -> a -> m (Maybe o)
nShot n (Body (K j) (K k) (K e)) a
  | n <= 0 = pure Nothing
  | otherwise = do
      initial <- j a
      ch <- commits n initial
      Const o <- e ch
      pure (Just o)
  where
    commits 1 ch = pure ch
    commits i ch = k (ch, a) >>= commits (i - 1)

-- | Nanoseconds retained in the clock's native signed 64-bit representation.
newtype Nanos = Nanos {unNanos :: Int64}
  deriving (Show, Read, Eq, Ord, Num, Real, Enum, Integral)

-- | Bytes sampled from the RTS allocation counter. Use @Sum Bytes@ for
-- additive accumulation.
newtype Bytes = Bytes {unBytes :: Word64}
  deriving (Show, Read, Eq, Ord, Num, Real, Enum, Integral)

-- | Read the raw monotonic timestamp used by the time meters and 'clock'.
-- Absolute values have no portable meaning; use 'elapsed' to compare readings.
-- Windows uses 'Monotonic'; other platforms use 'MonotonicRaw'.
stamp :: IO TimeSpec
#ifdef mingw32_HOST_OS
stamp = getTime Monotonic
#else
stamp = getTime MonotonicRaw
#endif

-- | Elapsed nanoseconds between two raw timestamps, including across a
-- negative nanosecond-field transition between seconds.
elapsed :: TimeSpec -> TimeSpec -> Nanos
elapsed (TimeSpec s0 n0) (TimeSpec s1 n1) = Nanos ((s1 - s0) * 1000000000 + n1 - n0)

allocationStamp :: IO Word64
allocationStamp = allocated_bytes <$> getRTSStats

-- | Force a major GC and then read the RTS allocation counter.
allocationGCStamp :: IO Word64
allocationGCStamp = performGC >> allocationStamp

readTimeSpace :: IO (TimeSpec, Word64)
readTimeSpace = (,) <$> stamp <*> allocationStamp

-- | Force a major GC before the paired timestamp and RTS sample. Consequently
-- the ending GC is inside the elapsed interval.
readTimeSpaceGC :: IO (TimeSpec, Word64)
readTimeSpaceGC = performGC >> readTimeSpace

hold :: a -> a
hold x = x
{-# NOINLINE hold #-}

-- | Sample continuous elapsed-time intervals around a pure function, forcing
-- its result to WHNF. Later intervals include runner work between commits.
time :: (a -> b) -> Meter IO (TimeSpec, ([Nanos], b)) a ([Nanos], b)
time f = timeIO (evaluate . f . hold)

-- | Sample continuous elapsed-time intervals around an IO action. Later
-- intervals include runner work between commits.
timeIO :: (a -> IO b) -> Meter IO (TimeSpec, ([Nanos], b)) a ([Nanos], b)
timeIO action =
  Body
    ( K \a -> do
        before <- stamp
        b <- action a
        after <- stamp
        pure (after, ([elapsed before after], b))
    )
    ( K \((before, (observations, _)), a) -> do
        b <- action a
        after <- stamp
        pure (after, (elapsed before after : observations, b))
    )
    (K \(_, (observations, b)) -> pure (Const (reverse observations, b)))

-- | Sample continuous raw RTS allocation deltas around an IO action. Requires
-- @+RTS -T@. No GC is forced. Deltas can include instrumentation allocation,
-- are affected by sharing and earlier work, and are not guaranteed totals for
-- the payload.
spaceIO :: (a -> IO b) -> Meter IO (Word64, ([Bytes], b)) a ([Bytes], b)
spaceIO action =
  Body
    ( K \a -> do
        before <- allocationStamp
        b <- action a
        after <- allocationStamp
        pure (after, ([Bytes (after - before)], b))
    )
    ( K \((before, (observations, _)), a) -> do
        b <- action a
        after <- allocationStamp
        pure (after, (Bytes (after - before) : observations, b))
    )
    (K \(_, (observations, b)) -> pure (Const (reverse observations, b)))

-- | Sample allocation like 'spaceIO', but run 'performGC' before every RTS
-- sample. GC adds work and latency; allocation deltas still include possible
-- instrumentation, depend on sharing and earlier work, and are not guaranteed
-- payload totals. Requires @+RTS -T@.
spaceGCIO :: (a -> IO b) -> Meter IO (Word64, ([Bytes], b)) a ([Bytes], b)
spaceGCIO action =
  Body
    ( K \a -> do
        before <- allocationGCStamp
        b <- action a
        after <- allocationGCStamp
        pure (after, ([Bytes (after - before)], b))
    )
    ( K \((before, (observations, _)), a) -> do
        b <- action a
        after <- allocationGCStamp
        pure (after, (Bytes (after - before) : observations, b))
    )
    (K \(_, (observations, b)) -> pure (Const (reverse observations, b)))

-- | Sample continuous elapsed-time and raw RTS allocation intervals around the
-- same IO action. Later intervals include runner work. Allocation attribution
-- has the same limits as 'spaceIO'. Requires @+RTS -T@.
timeSpaceIO :: (a -> IO b) -> Meter IO ((TimeSpec, Word64), ([(Nanos, Bytes)], b)) a ([(Nanos, Bytes)], b)
timeSpaceIO action =
  Body
    ( K \a -> do
        before <- readTimeSpace
        b <- action a
        after <- readTimeSpace
        pure (after, ([measure before after], b))
    )
    ( K \((before, (observations, _)), a) -> do
        b <- action a
        after <- readTimeSpace
        pure (after, (measure before after : observations, b))
    )
    (K \(_, (observations, b)) -> pure (Const (reverse observations, b)))
  where
    measure (t0, a0) (t1, a1) = (elapsed t0 t1, Bytes (a1 - a0))

-- | Sample time and allocation like 'timeSpaceIO', but run 'performGC' before
-- every paired sample. Each ending GC is inside that sample's elapsed time.
-- GC adds overhead; allocation can include instrumentation, depends on sharing
-- and earlier work, and is not a guaranteed payload total. Requires @+RTS -T@.
timeSpaceGCIO :: (a -> IO b) -> Meter IO ((TimeSpec, Word64), ([(Nanos, Bytes)], b)) a ([(Nanos, Bytes)], b)
timeSpaceGCIO action =
  Body
    ( K \a -> do
        before <- readTimeSpaceGC
        b <- action a
        after <- readTimeSpaceGC
        pure (after, ([measure before after], b))
    )
    ( K \((before, (observations, _)), a) -> do
        b <- action a
        after <- readTimeSpaceGC
        pure (after, (measure before after : observations, b))
    )
    (K \(_, (observations, b)) -> pure (Const (reverse observations, b)))
  where
    measure (t0, a0) (t1, a1) = (elapsed t0 t1, Bytes (a1 - a0))

-- | Perform exactly n raw clock reads and nothing else. Non-positive counts do
-- nothing. This warms the clock path, not the payload.
warmupClock :: Int -> IO ()
warmupClock n = replicateM_ n (void stamp)

-- | Run an IO payload exactly n times, discarding results. Non-positive counts
-- do nothing. The action controls forcing; a lazy action need not evaluate its
-- result. Repetition can change sharing, so this is distinct from both clock
-- warmup and the measured run.
warmupPayload :: Int -> (a -> IO b) -> a -> IO ()
warmupPayload n action a = replicateM_ n (void (action a))

-- | An interval instrument for pipeline markers. 'before' captures state and
-- 'after' observes it at a later boundary.
data Bracket m x y = Bracket
  { before :: m x,
    after :: x -> m y
  }

-- | Monotonic elapsed-time instrument for pipeline markers.
clock :: Bracket IO TimeSpec Nanos
clock = Bracket stamp (\origin -> elapsed origin <$> stamp)

-- | Raw RTS allocation instrument for pipeline markers. Requires @+RTS -T@;
-- deltas have the attribution limits described by 'spaceIO'.
allocation :: Bracket IO Word64 Bytes
allocation = Bracket allocationStamp (\origin -> Bytes . subtract origin <$> allocationStamp)

-- | GC-forced RTS allocation instrument for pipeline markers. Runs 'performGC'
-- before every sample. GC adds overhead; deltas have the attribution limits
-- described by 'spaceGCIO'. Requires @+RTS -T@.
allocationGC :: Bracket IO Word64 Bytes
allocationGC = Bracket allocationGCStamp (\origin -> Bytes . subtract origin <$> allocationGCStamp)

-- | Combined elapsed-time and raw allocation instrument for pipeline markers.
-- Requires @+RTS -T@.
timeSpace :: Bracket IO (TimeSpec, Word64) (Nanos, Bytes)
timeSpace = Bracket readTimeSpace measure
  where
    measure (t0, a0) = do
      (t1, a1) <- readTimeSpace
      pure (elapsed t0 t1, Bytes (a1 - a0))

-- | Combined GC-forced time and allocation instrument for pipeline markers.
-- Runs 'performGC' before every paired sample; the ending GC is inside the
-- elapsed interval. Allocation attribution follows 'timeSpaceGCIO'. Requires
-- @+RTS -T@.
timeSpaceGC :: Bracket IO (TimeSpec, Word64) (Nanos, Bytes)
timeSpaceGC = Bracket readTimeSpaceGC measure
  where
    measure (t0, a0) = do
      (t1, a1) <- readTimeSpaceGC
      pure (elapsed t0 t1, Bytes (a1 - a0))

-- | Pipeline measurement state: separate origins for the enclosing measurement
-- and current lap, with samples retained latest first internally.
data Pipeline x y = Pipeline
  { origin :: x,
    current :: x,
    samples :: Map String [y]
  }

-- | Pipeline state for monotonic time measurements.
type TimePipeline = Pipeline TimeSpec Nanos

record :: String -> y -> Map String [y] -> Map String [y]
record label y = Map.insertWith (++) label [y]

-- | All labelled pipeline samples, oldest first.
laps :: Pipeline x y -> Map String [y]
laps = Map.map reverse . samples

-- | Samples for one label, oldest first.
lapsFor :: String -> Pipeline x y -> [y]
lapsFor label = maybe [] reverse . Map.lookup label . samples

-- | Start an enclosing measurement and its first lap.
start :: (Monad m) => Bracket m x y -> FreeTrace (,) (K m) a (a, Pipeline x y)
start meter = Lift $ K $ \a -> do
  x <- before meter
  pure (a, Pipeline x x Map.empty)

-- | Record the current lap and start the next one.
lap :: (Monad m) => Bracket m x y -> String -> FreeTrace (,) (K m) (a, Pipeline x y) (a, Pipeline x y)
lap meter label = Lift $ K $ \(a, ws) -> do
  y <- after meter (current ws)
  x' <- before meter
  pure (a, ws {current = x', samples = record label y (samples ws)})

-- | Record the enclosing interval since 'start'. This is measured directly,
-- not folded from the laps.
stop :: (Monad m) => Bracket m x y -> String -> FreeTrace (,) (K m) (a, Pipeline x y) (a, Pipeline x y)
stop meter name = Lift $ K $ \(a, ws) -> do
  y <- after meter (origin ws)
  pure (a, ws {samples = record name y (samples ws)})

firstK :: (Functor m) => K m a b -> K m (a, c) (b, c)
firstK (K k) = K (\(a, c) -> fmap (\b -> (b, c)) (k a))

-- | Lift a base arrow while passing pipeline measurement state through.
carry :: (Functor m) => K m a b -> FreeTrace (,) (K m) (a, Pipeline x y) (b, Pipeline x y)
carry stage = Lift (firstK stage)

-- | Lift an existing circuit stage while passing pipeline measurement state
-- through.
carryT :: (Functor m, Trace t (K m)) => FreeTrace t (K m) a b -> FreeTrace (,) (K m) (a, Pipeline x y) (b, Pipeline x y)
carryT stage = Lift (firstK (eval stage))
