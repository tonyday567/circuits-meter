# Seismo — Streaming Performance Observation

> A meter as heart monitor. Watch the jitter; spot the anomaly.

## The Idea

Batch benchmarks (`ticks`, `ticksN`) collapse measurements into aggregates:
min, p50, avg. This hides the *shape* of performance over time. A GC pause, a
context switch, a cache miss — these show up as spikes in the raw stream.

**Seismo mode** streams each measurement as it happens. No aggregation. You
watch the line wiggle and learn to read the system's pulse.

```
run 1:   62µs
run 2:   63µs
run 3:   64µs
run 4:   1.2ms    ← GC happened here
run 5:   63µs
```

## The API

```haskell
-- | Stream measurements to a callback.
seismoK :: Meter s t -> (t -> IO ()) -> Kleisli IO a b -> Kleisli IO a b
seismoK m emit k = Kleisli \a -> do
  (t, b) <- runKleisli (meterK m k) a
  emit t
  pure b
```

The meter bookends each run; the callback fires with the measurement; the
payload continues unchanged. The observer is ambient — the computation doesn't
know it's being watched.

## Research Note: Parser Jitter

Running `many anyToken` over 10k chars, 50 runs, with `seismoK` printing each
nanosecond count:

```
244291
245583
240458
245833
243750
1207916   ← 5× spike
244041
246458
...
```

The spike is a minor GC. The list allocated by `many` fills a nursery generation;
when it dies, the collector pauses the world for ~1ms. Without seismo mode this
would be invisible inside the average.

## Hypothesis

Seismo mode turns `circuits-meter` from a benchmark tool into a *profiler*.
Instead of "how fast is this?" you ask "what's happening while this runs?"

Future directions:
- Ring buffer: keep last N measurements, dump on anomaly
- Delta mode: print `(current - previous)` to spot drift
- Spectrogram: bucket measurements by magnitude to see bimodal distributions

## Code

```haskell
{-# LANGUAGE BlockArguments #-}

import Circuit.Perf
import Circuit.Perf.Time
import Control.Arrow
import Text.Printf

heartbeat :: Meter Nanos Nanos
heartbeat = timeM

-- Print each measurement
seismo :: (Int -> IO ()) -> Meter s t -> (a -> b) -> a -> IO b
seismo emit m f a = do
  (t, b) <- once m f a
  emit (fromIntegral t)
  pure b

main :: IO ()
main = do
  let n = 50
  putStrLn "run  time"
  mapM_ (\i -> do
    t <- seismo (printf "%3d  %dns\n" i) heartbeat (*2) (5 :: Int)
    pure t
    ) [1..n]
```
