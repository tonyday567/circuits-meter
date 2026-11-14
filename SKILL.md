---
name: circuits-meter
description: Input-seeded repeated measurement and labelled pipeline metering through Circuit.Meter.
---

# circuits-meter — agent field guide

One public module owns the library:

```text
Circuit.Meter — Meter runners, observations, time/space instruments, pipeline markers
```

`Meter m ch a o` is a `Body (,) Const ch ch (K m) a o`. Body is the representation, not the public module taxonomy. Its first input runs `init`; later inputs run `commit`; `emit` runs once at the end of a non-empty run.

## core API

```haskell
oneShot :: Monad m => Meter m ch a o -> a -> m o
nShot :: Monad m => Int -> Meter m ch a o -> a -> m (Maybe o)
converge :: Monad m => Int64 -> (ch -> Bool) -> Meter m ch a o -> [a] -> m (Either o Text)

count :: Monad m => (a -> b) -> Meter m (Int, b) a (Int, b)
observe :: Monad m => (a -> b -> observation) -> (a -> b)
  -> Meter m ([observation], b) a ([observation], b)
```

`nShot <= 0` performs no init, commit, or emit. Observation channels retain only the latest `b`, not a second result tape. `named`/`namedN` prepend projected samples under a key, so their per-key lists are latest first.

`converge` checks the fresh channel, counts init as iteration one, emits once on success, and returns a textual failure on fuel or input exhaustion.

## time and space

```haskell
time :: (a -> b) -> Meter IO (TimeSpec, ([Nanos], b)) a ([Nanos], b)
timeIO :: (a -> IO b) -> Meter IO (TimeSpec, ([Nanos], b)) a ([Nanos], b)
spaceIO :: (a -> IO b) -> Meter IO (Word64, ([Bytes], b)) a ([Bytes], b)
timeSpaceIO :: (a -> IO b) -> Meter IO ((TimeSpec, Word64), ([(Nanos, Bytes)], b)) a ([(Nanos, Bytes)], b)
```

`time` forces WHNF. Use `timeIO` with `pure`, `evaluate`, or `evaluate . force` to select lazy, WHNF, or NF evaluation explicitly.

Intervals are continuous: n inputs need n+1 stamps, and later samples include inter-commit runner work. `Nanos` is signed `Int64`; elapsed time is calculated directly from `TimeSpec` fields. Allocation uses `GHC.Stats.allocated_bytes`, requires `+RTS -T`, and reports raw counter deltas.

## pipeline API

```haskell
start clock
  .> carry stage
  .> lap clock "stage"
  .> stop clock "total"
```

- `Bracket` supplies `before` and `after` legs for an interval instrument.
- `clock`, `allocation`, and `timeSpace` are the standard instruments.
- `carry` lifts `K`; `carryT` lifts `FreeTrace` while passing pipeline state.
- `Pipeline` retains separate enclosing and current-lap origins.
- `laps` and `lapsFor` return samples oldest first.
- `stop "total"` measures from the original start, not by summing laps.
- `TimePipeline` names the common `Pipeline TimeSpec Nanos` result without exposing clock details in consumer signatures.

The canonical consumer is `~/haskell/circuits-examples/app/Example/Words.hs`. Preserve its real open/read/close/format stages, explicit forcing, labelled observations, enclosing total, and lazy/separate-forced/fused comparison when changing this API.

## build and verification

```bash
cd ~/haskell/circuits-meter
fare gate --docspec
cabal run circuits-meter-axioma

cd ~/haskell/circuits-examples
cabal build all
cabal run circuits-examples-axioma

cd ~/machina/circuits-meter-machina
cabal build perf-explore
cabal run perf-explore -- sum --algorithm sumFuse --runs 1000 --warmup 1000 --nocheck
```

The core axioma pins first input, zero and exact runs, emit count, latest results, observation order, named prepend, convergence fuel, pipeline laps, enclosing totals, restart gaps, and repeated-label order.

## measurement traps

- Compile with optimisation. GHCi uses different evaluation and code generation.
- Warm up deliberately; clock warmup is not payload warmup.
- Do not compare continuous Body samples with independent two-stamp brackets as though they were identical measurements.
- Forcing between pipeline stages can prevent fusion and change the program being measured.
- A named whole-run statistic is not a labelled interval inside one pipeline run.
- Low-resolution clock readings and GC tails belong in the interpretation, not hidden behind a single average.

Historical Core and calibration notes under `examples/` retain their original measurements and commands as historical records, not current executable instructions.
