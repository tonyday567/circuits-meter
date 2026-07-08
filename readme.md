# circuits-meter

Performance measurement as a `Circuit`.

The four things you actually call:

```haskell
import Circuit.Meter.Time
import Circuit.Meter.Space

-- 1. Time a single pure function call.
tick :: (NFData b) => (a -> b) -> a -> IO (Nanos, b)

-- 2. Time @n@ calls and average the results.
ticksN :: (NFData b) => Int -> (a -> b) -> a -> IO (Nanos, b)

-- 3. Time @n@ 'IO' actions and average the results.
ticksION :: Int -> IO a -> IO (Nanos, a)

-- 4. Measure time and allocation together.
both timeM allocM :: Meter (Kleisli IO) (Nanos, Bytes) (Nanos, Bytes)
```

For raw per-run distributions, use `ticks` (pure) or `timesK` (Kleisli).
For custom meters, build a `Meter` with `mkMeter` and wrap an arrow with
`meterAction`.

## Baseline / calibration

`cabal run baseline` exercises a ladder of micro-benchmarks:
function-application floor, list primitives, polymorphism cost, and the
state-hiding (skolem) effect used by `foldl` / `Mealy`.

`examples/baseline-analytics.md` contains the calibrated numbers plus the
GHC Core for the tight sum loop and a C reference calibration.

See also [perf-circuits](https://github.com/tonyday567/perf-circuits), the
sibling repo with larger-scale circuit performance examples.
