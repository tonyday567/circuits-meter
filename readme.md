# circuits-meter

Performance measurement as a `Circuit`.

A `Meter` is a pair of arrows: one captures state before a computation, one
observes it after. `meterAction` sandwiches your payload between them, giving you
a `Loop` (a trace-ready circuit) polymorphic in the tensor. Time and allocation
meters compose in parallel via `both`.

## The four things you call

```haskell
import Circuit.Meter.Time

-- 1. Time a single pure function call.
tick :: (NFData b) => (a -> b) -> a -> IO (Nanos, b)

-- 2. Time n calls and average.
ticksN :: (NFData b) => Int -> (a -> b) -> a -> IO (Nanos, b)

-- 3. Time n IO actions and average (100 warmup calls first).
ticksION :: Int -> IO a -> IO (Nanos, a)

-- 4. Time and allocation together.
both timeX allocX :: Meter (Kleisli IO) (Nanos, Bytes) (Nanos, Bytes)
```

For per-run distributions use `ticks` (pure) or `timesK` (Kleisli). For custom
meters, build a `Meter` with `mkMeter` and wrap your arrow with `meterAction`.

## Stopwatch pipelines

Drop named markers into a left-to-right pipeline and the timing log rides
alongside the payload on a `(,)` wire. This is where you see laziness effects
directly — cost that should be on one interval slides into another and the meter
makes it explicit.

```haskell
import Circuit
import Circuit.Category ((.>))
import Circuit.Meter.Stopwatch
import Circuit.Meter.Time (timeX)
import Control.Arrow (Kleisli (..))

wordPipeline :: Loop (,) (Kleisli IO) FilePath (String, Watches Nanos Nanos)
wordPipeline =
  start timeX "total"
    .> carryT openf
    .> lap timeX "opened"
    .> carryT readAndCount
    .> carry forceMap          -- stop laziness from shifting cost
    .> lap timeX "read"
    .> carry post
    .> lap timeX "closed"
    .> carryT formatfForced
    .> stop timeX "total"
```

Without the `forceMap` stage the counting work is lazy, so it's forced later by
`formatfForced` and appears under "total":

| interval | without forceMap | with forceMap |
|---|---|---|
| opened  | 0.56 ms | 0.30 ms |
| read    | 3.52 ms | **51.28 ms** |
| closed  | 0.01 ms | 0.01 ms |
| total   | **50.31 ms** | 0.54 ms |

The stopwatch wire makes the shift explicit: the same program, with one extra
`force` point, reports a completely different bottleneck.

## Laziness, WHNF, NF

The library doesn't decide how much to force. That's just another Kleisli stage
between markers:

```haskell
carry (Kleisli (pure . f))                  -- return a thunk
carry (Kleisli (evaluate . f))              -- force to WHNF
carry (Kleisli (evaluate . force . f))      -- force to NF
```

This keeps maximum visibility: you choose the evaluation boundary, and the meter
records what happens there.

## Function-composition experiment

The same marker style exposes fusion. Compare separate stages against a fused
composition on 100,000 words:

| variant | split | lower | filter | total |
|---|---|---|---|---|
| separate, force at end | <0.001 ms | <0.001 ms | <0.001 ms | **448 ms** |
| separate, force between | 316 ms | 119 ms | 23 ms | 458 ms |
| fused, single force | — | — | — | **329 ms** |

Without intermediate forces all cost collapses into the final stop. With forces
the cost distributes across stages. The fused version is faster because GHC can
combine `splitWords >>> lowerWords >>> noEmpties` into a single pass.

## Space analytics

Replace `timeX` with `allocX` or combine them with `both`:

```haskell
import Circuit.Meter.Space
import Circuit.Meter (both)

meterIt (both timeX allocX) "chunk" :: Loop (,) (Kleisli IO) a (b, Watches (Nanos, Bytes) (Nanos, Bytes))
```

Every stopwatch marker records nanoseconds and allocated bytes at that point.

### allocX vs allocGC

`allocX` reads `getRTSStats.allocated_bytes` — a cumulative counter that only
updates at GC time. Allocations that fit in the nursery between GCs are
invisible to it. It's fast but the numbers are a lagging indicator:

```
start allocX → 64 MB (counter frozen from last GC)
... allocate 6 MB in nursery ...
stop allocX  → 64 MB (counter unchanged — no GC ran)
delta = 0
```

`allocGC` forces a major GC (`performGC`) at both start and stop boundaries.
This gives accurate per-interval allocation at the cost of GC overhead:

```
start allocGC → GC → 0 MB (counter reset)
... allocate 6 MB ...
stop allocGC  → GC → 6 MB (counter updated by GC)
delta = 6 MB
```

Use `allocX` for lightweight measurement where coarse numbers are acceptable.
Use `allocGC` when you need accurate per-stage allocation and can tolerate the
GC pauses. Both meters live in `Circuit.Meter.Space`.

### The laziness experiment

`circuits-meter-observe/app/Laziness.hs` demonstrates time+space metering on
lazy / WHNF / NF evaluation of a 100k-element list:

```
=== time only ===
drop:    51 µs   (thunk creation, pure allocation)
WHNF:   280 µs   (spine forced, compute appears)
count:  792 µs   (spine forced via fold)
sum:   1.6 ms    (elements forced, compute increases)
```

With `both timeX allocGC` the space channel confirms the split: `drop` shows
allocation with near-zero compute; `sum` shows compute with near-zero
allocation. The stopwatch reveals *where* the work actually happens — and the
space channel distinguishes allocation from computation.

Run with `+RTS -T -RTS` to enable GC stats:

```
cabal run laziness -- +RTS -T -RTS
```

## Words: the R&D laboratory

The [words](https://github.com/tonyday567/words) project is where circuits-meter
is exercised. It's a word-counting pipeline built as a `Loop` circuit with
stopwatch markers at every stage — open file, read-and-count loop, close, format.
The pipeline is the readme's running example.

Three executables in words:

- **`word`** — the full stopwatch pipeline (`readme`/stopwatch-pipelines example).
- **`producer-consumer`** — open → readAll → close as explicit `Loop` stages, no
  metering. The simplest complete circuit.
- **`perf-bench`** (in circuits-meter) — benchmarks the measurement machinery
  itself: clock overhead, IORef vs trace-delim, meterAction overhead, and
  simultaneous time+space via `both`.

## Runners in the repo

- **`nub`** — `Data.List.nub` as a quadratic reference shape. Times nub, Set-nub,
  and sort-nub against input sizes, fits a quadratic model, and reports R². The
  point is to exercise `ticks`, not to win a benchmark.

## Custom meters

```haskell
import Circuit.Meter
import Control.Arrow (Kleisli (..))

myMeter :: Meter (Kleisli IO) State Reading
myMeter = mkMeter captureState observeState

metered :: Loop t (Kleisli IO) a (Reading, b)
metered = meterAction myMeter myArrow
```

`meterAction` is tensor-agnostic — it only needs `Category` + `Strong` from the
base arrow, so the result lifts into pipelines using `(,)` or `Either` without
changing the combinator.

## What's not here

Heavy micro-benchmark harnesses (`baseline`, `alloc-probe`) moved to
[perf-circuits](https://github.com/tonyday567/perf-circuits). This repo is the
public metering library plus the `perf-bench` and `nub` runners.
