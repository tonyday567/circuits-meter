{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}

-- | Input-seeded meters, repeated runners, and labelled pipeline measurement.
module Circuit.Meter
  ( Meter,
    oneShot,
    nShot,
    named,
    namedN,
    converge,
    count,
    observe,
    Nanos (..),
    Bytes (..),
    time,
    timeIO,
    spaceIO,
    timeSpaceIO,
    Bracket (..),
    clock,
    allocation,
    timeSpace,
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
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import GHC.Stats (allocated_bytes, getRTSStats)
import System.Clock
import Prelude hiding (init)

-- | A diagonal, input-seeded machine whose final observation is emitted from
-- the fresh channel.
type Meter m ch a o = Body (,) Const ch ch (K m) a o

-- | Initialise and emit, with no commit leg.
oneShot :: (Monad m) => Meter m ch a o -> a -> m o
oneShot (Body (K j) _ (K e)) a = do
  ch <- j a
  Const o <- e ch
  pure o

-- | Initialise with the first input and commit the remaining n−1 identical
-- inputs directly. Zero and negative counts produce no observation.
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

-- | Prepend a projected one-shot observation under a name. Each key's samples
-- are latest first.
named :: (Monad m, Ord k) => (o -> stats) -> k -> Meter m ch a o -> a -> Map k [stats] -> m (Maybe o, Map k [stats])
named = namedN 1

-- | Prepend a projected n-shot observation under a name. Each key's samples
-- are latest first; empty runs leave the map unchanged.
namedN :: (Monad m, Ord k) => Int -> (o -> stats) -> k -> Meter m ch a o -> a -> Map k [stats] -> m (Maybe o, Map k [stats])
namedN n project key meter a observations = do
  result <- nShot n meter a
  let observations' = case result of
        Nothing -> observations
        Just o ->
          let stat = project o
           in Map.alter (Just . maybe [stat] (stat :)) key observations
  pure (result, observations')

convergenceFailure :: Int64 -> Either o Text
convergenceFailure n = Right (Text.pack ("function did not converge after " <> show n <> " iterations"))

-- | Stop when the channel satisfies a predicate, or when fuel or inputs run
-- out. Init counts as the first iteration.
converge :: (Monad m) => Int64 -> (ch -> Bool) -> Meter m ch a o -> [a] -> m (Either o Text)
converge fuel predicate (Body (K j) (K k) (K e)) inputs
  | fuel <= 0 = pure (convergenceFailure 0)
  | otherwise = case inputs of
      [] -> pure (convergenceFailure 0)
      a : rest -> do
        ch <- j a
        if predicate ch
          then success ch
          else continue 1 ch rest
  where
    success ch = do
      Const o <- e ch
      pure (Left o)
    continue n ch as
      | n >= fuel = pure (convergenceFailure n)
      | otherwise = case as of
          [] -> pure (convergenceFailure n)
          a : rest -> do
            ch' <- k (ch, a)
            let n' = n + 1
            if predicate ch'
              then success ch'
              else continue n' ch' rest

-- | Count inputs and retain the latest result.
count :: (Monad m) => (a -> b) -> Meter m (Int, b) a (Int, b)
count f =
  Body
    (K \a -> pure (1, f a))
    (K \((n, _), a) -> pure (n + 1, f a))
    (K \ch -> pure (Const ch))

-- | Append an arbitrary observation of each result and retain only the latest
-- result in the channel.
observe :: (Monad m) => (a -> b -> observation) -> (a -> b) -> Meter m ([observation], b) a ([observation], b)
observe observation f =
  Body
    (K \a -> let b = f a in pure ([observation a b], b))
    (K \((observations, _), a) -> let b = f a in pure (observation a b : observations, b))
    (K \(observations, b) -> pure (Const (reverse observations, b)))

-- | Nanoseconds retained in the clock's native signed 64-bit representation.
newtype Nanos = Nanos {unNanos :: Int64}
  deriving (Show, Read, Eq, Ord, Num, Real, Enum, Integral)

-- | Bytes sampled from the RTS allocation counter.
newtype Bytes = Bytes {unBytes :: Word64}
  deriving (Show, Read, Eq, Ord, Num, Real, Enum, Integral)

instance Semigroup Bytes where
  (<>) = (+)

instance Monoid Bytes where
  mempty = 0

stamp :: IO TimeSpec
#ifdef mingw32_HOST_OS
stamp = getTime Monotonic
#else
stamp = getTime MonotonicRaw
#endif

elapsed :: TimeSpec -> TimeSpec -> Nanos
elapsed (TimeSpec s0 n0) (TimeSpec s1 n1) = Nanos ((s1 - s0) * 1000000000 + n1 - n0)

allocationStamp :: IO Word64
allocationStamp = allocated_bytes <$> getRTSStats

hold :: a -> a
hold x = x
{-# NOINLINE hold #-}

sampled :: (a -> IO b) -> IO stamp -> (stamp -> stamp -> observation) -> Meter IO (stamp, ([observation], b)) a ([observation], b)
sampled action readStamp measure =
  Body
    ( K \a -> do
        before <- readStamp
        b <- action a
        after <- readStamp
        pure (after, ([measure before after], b))
    )
    ( K \((before, (observations, _)), a) -> do
        b <- action a
        after <- readStamp
        pure (after, (measure before after : observations, b))
    )
    (K \(_, (observations, b)) -> pure (Const (reverse observations, b)))

-- | Sample continuous elapsed-time intervals around a pure function, forcing
-- its result to WHNF. Later intervals include runner work between commits.
time :: (a -> b) -> Meter IO (TimeSpec, ([Nanos], b)) a ([Nanos], b)
time f = timeIO (evaluate . f . hold)

-- | Sample continuous elapsed-time intervals around an IO action. Later
-- intervals include runner work between commits.
timeIO :: (a -> IO b) -> Meter IO (TimeSpec, ([Nanos], b)) a ([Nanos], b)
timeIO action = sampled action stamp elapsed

-- | Sample continuous RTS allocation intervals around an IO action. Requires
-- @+RTS -T@; the samples are raw counter deltas, not isolated workload totals.
spaceIO :: (a -> IO b) -> Meter IO (Word64, ([Bytes], b)) a ([Bytes], b)
spaceIO action = sampled action allocationStamp (\before after -> Bytes (after - before))

-- | Sample continuous elapsed-time and RTS allocation intervals around the
-- same IO action. Requires @+RTS -T@.
timeSpaceIO :: (a -> IO b) -> Meter IO ((TimeSpec, Word64), ([(Nanos, Bytes)], b)) a ([(Nanos, Bytes)], b)
timeSpaceIO action = sampled action readTimeSpace measure
  where
    measure (t0, a0) (t1, a1) = (elapsed t0 t1, Bytes (a1 - a0))

-- | An interval instrument for pipeline markers. 'before' captures state and
-- 'after' observes it at a later boundary.
data Bracket m x y = Bracket
  { before :: m x,
    after :: x -> m y
  }

readTimeSpace :: IO (TimeSpec, Word64)
readTimeSpace = (,) <$> stamp <*> allocationStamp

-- | Monotonic elapsed-time instrument for pipeline markers.
clock :: Bracket IO TimeSpec Nanos
clock = Bracket stamp (\origin -> elapsed origin <$> stamp)

-- | RTS allocation instrument for pipeline markers. Requires @+RTS -T@.
allocation :: Bracket IO Word64 Bytes
allocation = Bracket allocationStamp (\origin -> Bytes . subtract origin <$> allocationStamp)

-- | Combined elapsed-time and allocation instrument for pipeline markers.
-- Requires @+RTS -T@.
timeSpace :: Bracket IO (TimeSpec, Word64) (Nanos, Bytes)
timeSpace = Bracket readTimeSpace measure
  where
    measure (t0, a0) = do
      (t1, a1) <- readTimeSpace
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
