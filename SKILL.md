---
name: circuits-meter
description: Performance measurement as a Circuit. Time, space, and stopwatch metering over Kleisli arrows and traced monoidal categories.
---

# circuits-meter — agent field guide

Performance measurement reimagined as a traced monoidal category. A `Meter` is
a pair of arrows (`start` and `stop`) that bracket a payload arrow. The library
provides time, allocation, and stopwatch/pipeline meters that compose through
the `circuits` `Trace` abstraction.

## module map

```
Circuit.Meter           — Meter type, mkMeter, both, meterAction, hold
Circuit.Meter.Time      — timeX, tick, ticks, ticksN, ticksION, warmup, timesK
Circuit.Meter.Space     — allocX, SpaceStats, Bytes
Circuit.Meter.Stopwatch — Watches, start/lap/stop, carry/carryT, meterIt/timeIt
```

`circuits-meter` depends on `circuits` for the `Trace` class and Kleisli
instances. `import Circuit` is usually enough.

## build and run

```bash
cd ~/haskell/circuits-meter

# Library + all executables
cabal build

# Micro-benchmark harness (clock overhead, IORef loop, trace loop, meterAction, both)
cabal run perf-bench -- --runs 100000 --warmup 1000

# Baseline / calibration (function-call floor, list primitives, polymorphism, skolem folds)
cabal run baseline -- --size 100000 --count 10000000 --runs 100

# Write distribution SVGs
cabal run baseline -- --size 100000 --count 10000000 --runs 100 --chartdir other/charts

# Nub / deduplication example
cabal run nub -- --help

# Allocation probe
cabal run alloc-probe -- +RTS -T
```

All benchmark executables build with `-O2 -rtsopts`. Run with `+RTS -s` for
allocation/GC stats or `+RTS -T` for `getRTSStatsEnabled` APIs.

## the four public time runners

```haskell
import Circuit.Meter.Time

-- Time a single pure function call, forcing result to NF.
tick :: (NFData b) => (a -> b) -> a -> IO (Nanos, b)

-- Time n calls and average.
ticksN :: (NFData b) => Int -> (a -> b) -> a -> IO (Nanos, b)

-- Time n IO actions and average.
ticksION :: Int -> IO a -> IO (Nanos, a)

-- Meter a Kleisli arrow repeated n times; returns raw distribution + last result.
timesK :: Int -> Int -> Meter (Kleisli IO) x y -> Kleisli IO c d -> Kleisli IO c ([y], d)
```

For raw distributions use `ticks` (pure) or `timesK` (Kleisli).

## meters and composition

```haskell
import Circuit.Meter
import Circuit.Meter.Time (timeX)
import Circuit.Meter.Space (allocX)

-- Time meter: start reads the monotonic clock, stop reads it again and subtracts.
timeX :: Meter (Kleisli IO) Nanos Nanos

-- Allocation meter: RTS allocated-bytes delta.
allocX :: Meter (Kleisli IO) Bytes Bytes

-- Run two meters simultaneously.  State wires are independent.
both timeX allocX :: Meter (Kleisli IO) (Nanos, Bytes) (Nanos, Bytes)

-- Sandwich a Kleisli arrow between a meter's start and stop.
meterAction timeX (Kleisli action) :: Trace t (Kleisli IO) a (Nanos, b)
```

Custom meters are built with `mkMeter`:

```haskell
myMeter :: Meter (Kleisli IO) MyState MyMeasure
myMeter = mkMeter captureState measureDelta
```

## stopwatch pipeline

Drop named markers into a left-to-right pipeline. The timing log travels on a
`(,)` wire alongside the payload.

```haskell
import Circuit
import Circuit.Meter.Stopwatch
import Circuit.Meter.Time (timeX)
import Control.Arrow (Kleisli (..))
import Control.Category ((>>>))

pipeline :: Trace (,) (Kleisli IO) FilePath (String, Watches Nanos Nanos)
pipeline =
  start timeX "total"
    >>> carryT openf
    >>> lap timeX "opened"
    >>> carryT readAndCount
    >>> carry forceMap
    >>> lap timeX "read"
    >>> carry post
    >>> lap timeX "closed"
    >>> carryT formatfForced
    >>> stop timeX "total"
```

- `start m name` begins an active watch.
- `lap m label` records an interval since the previous `start`/`lap` under
  `label` and begins a fresh interval.
- `stop m name` records the final interval under `name` and keeps the log.
- `carry` lifts a Kleisli arrow while passing the timing wire through unchanged.
- `carryT` lifts an existing `Trace` stage into the cartesian timing wire.

Force boundaries explicitly so laziness does not shift cost between stages:

```haskell
carry (Kleisli (pure . f))            -- return a thunk
carry (Kleisli (evaluate . f))        -- force to WHNF
carry (Kleisli (evaluate . force . f)) -- force to NF
```

## the perf-explore pattern

1. **Compile, don't interpret.** GHCi is 10–100× slower and uses a different
   compilation strategy. Always `cabal run` or `cabal build` with `-O2`.

2. **Tight loop.** Measure one thing, many times. The `tick`/`ticksN` pattern
   reads the clock, runs the action, forces the result to NF, reads the clock
   again, and returns the delta.

3. **Warmup.** First measurements are cold (cache, GC nursery). `warmup 500-1000`
   before timing, or use `ticksN`/`ticksION` which include warmup.

4. **Percentiles, not averages.** Averages are skewed by GC pauses and OS
   scheduling. Report p10/p50/p90. `baseline` uses `tdigest` for percentile
   estimates and spike detection.

5. **RTS options.** Build with `-rtsopts` and run with `+RTS -s` for
   allocation/GC stats. Use `+RTS -A64M` to reduce GC frequency or `+RTS -I0`
   to disable idle GC for critical measurements.

## the perf-bench harness

`cabal run perf-bench` exercises five measurements:

1. **clock overhead** — back-to-back `nanos` reads. Noise floor.
2. **whileM_** — `IORef` counter loop. Control group.
3. **trace-delim** — `Trace (Kleisli IO) Either` counting loop via
   `prompt#`/`control0#`.
4. **meterAction-loop** — the trace loop measured with `Circuit.Meter.meterAction`.
5. **both** — simultaneous time + allocation on a single shot.

Representative numbers (Apple Silicon M-series, GHC 9.14.1, `-O2`):

| benchmark | per iteration | ratio |
|---|---|---|
| clock overhead | ~125 ns | 1× |
| whileM_ (IORef) | ~8 ns | baseline |
| trace-delim | ~18 ns | ~2.25× |

The delta (~10 ns per step) is the cost of the delimited-continuation
primitives: prompt tag allocation, continuation capture, and trampoline dispatch.

## Core inspection

Dump GHC Core to understand what the compiler actually produces:

```bash
cabal build perf-bench \
  --ghc-options="-ddump-simpl -ddump-to-file \
                 -dsuppress-all -dno-suppress-type-signatures \
                 -fforce-recomp -O2"
```

Output goes to:
```
dist-newstyle/build/.../perf-bench-tmp/app/Main.dump-simpl
```

See `examples/core-analysis.md` for a walkthrough of reading Core for the
tight sum loop, `runTrace`, `timesK`, `reify`, and `hold`.

### quick checklist

**Worker/wrapper.** GHC creates unboxed workers (prefixed `$w`) that operate on
`Int#`, `State# RealWorld`. No heap allocation for arithmetic. If you see `I#`
boxing in a hot loop, something is wrong.

**Primops.** `prompt#`, `control0#`, `newPromptTag#` appear directly in Core —
no library abstraction survives compilation.

**Letrec loops.** `joinrec { $wgo = \... -> ... jump $wgo ... }` is a
 tail-recursive loop compiled to a jump. Good.

**INLINE/NOINLINE.** `NOINLINE` on benchmark entry points prevents GHC from
inlining the entire benchmark into the measurement loop and dead-code
eliminating the work. Without it, GHC can constant-fold `sum [1..1000]` to
`pure 1000`.

**`hold`.** `hold :: a -> a` is `NOINLINE` and identity. It stops GHC from
floating a pure computation past the meter boundary. Search for `hold` in Core;
it appears as `f (hold x)` inside a `let` binding.

## gotchas

### repl timing is meaningless

`cabal repl` timing is 10–100× slower than compiled code. Always use
`cabal run` or a compiled binary for benchmarks.

### NOINLINE prevents dead code elimination

See the Core checklist above. The compiler is smart enough to compute constants
at compile time if you let it.

### clock resolution

`MonotonicRaw` on macOS has ~42 ns resolution but `getTime` itself takes ~60 ns
per call (user→kernel→user). Two calls ≈ 125 ns. Don't try to measure anything
below ~200 ns per iteration reliably.

### GC interference

GC pauses skew averages. Use p50 (median) and p99 tails, not means. For
critical measurements, `+RTS -A64M` or `+RTS -I0`.

### O2 matters

`-O1` is the cabal default. `-O2` enables more aggressive inlining and
specialisation. For tight loops, the difference can be 2–5×.

## sibling libraries

- **circuits** — free traced monoidal categories; `circuits-meter` builds on its
  `Trace` class and Kleisli instances.
- **circuits-parser** — `Trace (->) Either` parsers.
- **perf-circuits** — larger-scale circuit performance examples.

## further reading

- `readme.md` — public-facing overview and stopwatch example.
- `examples/baseline-analytics.md` — calibrated numbers, Core snippets, C
  reference, and the space/GC story behind `polySum`'s fat tail.
- `examples/core-analysis.md` — reading Core for circuit primitives.
