---
name: circuits-meter
description: Input-seeded repeated measurement and labelled pipeline metering through Circuit.Meter.
---

# circuits-meter — agent field guide

One public module owns the library:

```text
Circuit.Meter — nShot, raw clock access, time/space meters, warmup, pipeline markers
```

`Meter m ch a o` is a `Body (,) Const ch ch (K m) a o`. Body is the representation, not the public module taxonomy. Its first input runs `init`; later inputs run `commit`; `emit` runs once at the end of a non-empty run.

## core runner

```haskell
nShot :: Monad m => Int -> Meter m ch a o -> a -> m (Maybe o)
```

Positive runs use a direct init plus n−1 commit loop with no input tape. `nShot <= 0` performs no init, commit, or emit. Use `nShot 1` for a one-input boundary; there is no `oneShot` runner.

`count` and `converge` are pedagogic witnesses in `~/haskell/circuits-examples/app/Example/Meter.hs`, not core vocabulary. The example count has a `(Sum Int, b)` channel and combines old and new contributions, so `First`, `Last`, `Sum`, and list semigroups select retention.

## raw clock and repeated time/space

```haskell
stamp :: IO TimeSpec
elapsed :: TimeSpec -> TimeSpec -> Nanos

time :: (a -> b) -> Meter IO (TimeSpec, ([Nanos], b)) a ([Nanos], b)
timeIO :: (a -> IO b) -> Meter IO (TimeSpec, ([Nanos], b)) a ([Nanos], b)
spaceIO :: (a -> IO b) -> Meter IO (Word64, ([Bytes], b)) a ([Bytes], b)
spaceGCIO :: (a -> IO b) -> Meter IO (Word64, ([Bytes], b)) a ([Bytes], b)
timeSpaceIO :: (a -> IO b) -> Meter IO ((TimeSpec, Word64), ([(Nanos, Bytes)], b)) a ([(Nanos, Bytes)], b)
timeSpaceGCIO :: (a -> IO b) -> Meter IO ((TimeSpec, Word64), ([(Nanos, Bytes)], b)) a ([(Nanos, Bytes)], b)
```

Use `stamp` and `elapsed` rather than treating `before clock` as public clock access or duplicating the Windows fallback. `time` forces WHNF. Use `timeIO` with `pure`, `evaluate`, or `evaluate . force` to select lazy, WHNF, or NF evaluation explicitly.

Intervals are continuous: n inputs need n+1 stamps, and later samples include inter-commit runner work. `Nanos` is signed `Int64`. `Bytes` and `Nanos` have no additive semigroup instances; use `Sum Bytes` or `Sum Nanos` explicitly.

Allocation requires `+RTS -T`. Raw deltas are not isolated workload totals. GC-forced variants run `performGC` before every RTS sample; GC adds overhead, while instrumentation allocation, sharing, earlier work, and non-total attribution remain explicit. Combined GC-forced intervals include the ending GC in elapsed time.

## explicit warmup

```haskell
warmupClock :: Int -> IO ()
warmupPayload :: Int -> (a -> IO b) -> a -> IO ()
```

Clock warmup reads only the clock. Payload warmup runs only the supplied action and discards results. Neither calls the other and neither has a default count. Non-positive counts do nothing. Payload forcing and sharing remain the caller's responsibility.

## pipeline API

```haskell
start clock
  .> carry stage
  .> lap clock "stage"
  .> stop clock "total"
```

- `Bracket` supplies `before` and `after` legs for an interval instrument.
- `clock`, `allocation`, and `timeSpace` are raw instruments.
- `allocationGC` and `timeSpaceGC` force GC before every sample.
- `carry` lifts `K`; `carryT` lifts `FreeTrace` while passing pipeline state.
- `Pipeline` retains separate enclosing and current-lap origins.
- `laps` and `lapsFor` return samples oldest first.
- `stop "total"` measures from the original start, not by summing laps.
- `TimePipeline` names the common `Pipeline TimeSpec Nanos` result without exposing clock details in consumer signatures.

The canonical pipeline consumer is `~/haskell/circuits-examples/app/Example/Words.hs`. Preserve its real open/read/close/format stages, explicit forcing, labelled observations, enclosing total, and lazy/separate-forced/fused comparison when changing this API.

## build and verification

```bash
cd ~/haskell/circuits-meter
fare gate --docspec
cabal run circuits-meter-axioma -- +RTS -T
cabal haddock

cd ~/haskell/circuits-examples
fare gate --docspec
cabal run circuits-examples-axioma
```

The 12 core witnesses pin first input, zero/exact runs, emit count, raw elapsed arithmetic, continuous samples, pipeline laps/totals/gaps/order, raw and GC-forced allocation, and separate warmup counts. The examples oracle includes `example-meter` and the canonical Words execution.

## measurement traps

- Compile with optimisation. GHCi uses different evaluation and code generation.
- Do not mix clock and payload warmup or assume either has a default.
- Do not compare continuous Body samples with independent two-stamp brackets as though they were identical measurements.
- Forcing between pipeline stages can prevent fusion and change the program being measured.
- GC-forced samples are not pure workload attribution: GC, instrumentation, sharing, and earlier work still matter.
- Low-resolution clock readings and GC tails belong in the interpretation, not hidden behind a single average.

Historical Core and calibration notes under `examples/` retain their original measurements and commands as historical records, not current executable instructions.
