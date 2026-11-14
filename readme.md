# circuits-meter

Performance measurement as an input-seeded circuit machine, with labelled pipeline observations.

`Circuit.Meter` is self-contained. Its `Meter` is a diagonal `Circuit.Machine.Body`: `init` handles the first input, `commit` handles later inputs, and `emit` observes the final fresh channel.

## Repeated measurement

```haskell
import Circuit.Meter

samples <- nShot 1000 (timeIO action) input
-- Maybe ([Nanos], b)
```

Positive `nShot n` is `init → commit × (n−1) → emit`, using the same input without allocating an input tape. Non-positive counts return `Nothing` without running any leg. There is deliberately no separate one-shot runner: use `nShot 1` when that boundary is needed.

Pedagogic `count` and fuelled `converge` witnesses live in `example-meter` in `circuits-examples`. Their monoidal count combines each new result into the old channel; `First`, `Last`, `Sum`, and list wrappers select retention.

## Raw time and allocation

- `stamp` exposes the monotonic timestamp used by the library, including its Windows fallback; `elapsed` computes `Nanos` between two stamps.
- `time` measures a pure function and forces its result to WHNF.
- `timeIO` measures an IO action; use it to choose lazy, WHNF, or NF evaluation explicitly.
- `spaceIO` samples raw RTS allocation deltas and requires `+RTS -T`.
- `spaceGCIO` runs `performGC` before every allocation sample.
- `timeSpaceIO` measures raw time and allocation around the same action.
- `timeSpaceGCIO` forces GC before every paired sample; each ending GC lies inside that elapsed interval.

Timing is continuous: the previous ending stamp begins the next interval. This uses n+1 clock reads for n inputs; samples after the first include runner work between commits.

Raw and GC-forced allocation deltas are not guaranteed workload totals. They can include instrumentation allocation and depend on sharing and work completed before the interval. Forced GC refreshes the RTS sample but adds overhead; the GC-forced combined meters include the ending GC in elapsed time.

For NF measurement:

```haskell
nShot runs (timeIO (evaluate . force . f)) input
```

## Explicit warmup

Warmup kinds are separate and have no defaults:

```haskell
warmupClock 1000                         -- clock reads only
warmupPayload 100 (evaluate . force . f) input -- payload runs only
samples <- nShot 1000 (timeIO (evaluate . force . f)) input
```

`warmupPayload` discards results, so its action determines what is actually evaluated. Repetition can change sharing and therefore the measured workload. Non-positive warmup counts do nothing.

## Pipeline measurement

Pipeline markers observe stages inside one actual execution. The canonical example is `example-words` in `circuits-examples`:

```haskell
wordPipeline =
  start clock
    .> carryT openf
    .> lap clock "opened"
    .> carryT readAndCount
    .> carry forceMap
    .> lap clock "read"
    .> carry post
    .> lap clock "closed"
    .> carryT formatfForced
    .> lap clock "format"
    .> stop clock "total"
```

`carry` lifts a `K` stage while passing measurement state through. `carryT` lifts a `FreeTrace` stage. `laps` returns every label's samples oldest first; `lapsFor` selects one label.

`stop` measures the enclosing interval directly from `start`. It does not sum the laps, so marker and restart gaps remain enclosed. `clock`, `allocation`, and `timeSpace` supply raw interval instruments. `allocationGC` and `timeSpaceGC` force GC before each sample and have the same attribution limits as their repeated counterparts.

Words also compares separate lazy stages, separately forced stages, and a fused stage. The labels expose where laziness moves work and how forcing boundaries affect fusion.

## Build and run

```bash
cd ~/haskell/circuits-meter
cabal build all
cabal run circuits-meter-axioma -- +RTS -T

cd ~/haskell/circuits-examples
cabal build all
cabal run circuits-examples-axioma
```

The core executable runs 12 meter witnesses. Use compiled optimised executables for performance claims; GHCi timing is not benchmark evidence. Allocation measurement needs `-rtsopts` at build time and `+RTS -T` at run time.
