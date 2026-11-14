# circuits-meter

Performance measurement as an input-seeded circuit machine, with labelled pipeline observations.

`Circuit.Meter` is self-contained. Its `Meter` is a diagonal `Circuit.Machine.Body`: `init` handles the first input, `commit` handles later inputs, and `emit` observes the final fresh channel.

## Repeated measurement

```haskell
import Circuit.Meter

result <- oneShot (count (+ 1)) (41 :: Int)
-- (1,42)

samples <- nShot 1000 (timeIO action) input
-- Maybe ([Nanos], b)
```

`oneShot` is `init → emit`. Positive `nShot n` is `init → commit × (n−1) → emit`, using the same input without allocating an input tape. Non-positive counts return `Nothing` without running any leg.

`observe` retains chronological observations and only the latest payload result. `named` and `namedN` accumulate projected statistics under a key, latest first. `converge` accepts varying inputs, checks the channel after every init/commit, and counts init as iteration one.

## Time and space

- `time` measures a pure function and forces its result to WHNF.
- `timeIO` measures an IO action; use it to choose lazy, WHNF, or NF evaluation explicitly.
- `spaceIO` samples the RTS allocation counter and requires `+RTS -T`.
- `timeSpaceIO` measures both around the same action and also requires `+RTS -T`.

Timing is continuous: the previous ending stamp begins the next interval. This uses n+1 clock reads for n inputs; samples after the first include runner work between commits. Allocation samples are raw RTS counter deltas, not guaranteed isolated workload totals.

For NF measurement:

```haskell
nShot runs (timeIO (evaluate . force . f)) input
```

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

`stop` measures the enclosing interval directly from `start`. It does not sum the laps, so marker and restart gaps remain enclosed. `clock`, `allocation`, and `timeSpace` supply interval instruments for these markers.

Words also compares separate lazy stages, separately forced stages, and a fused stage. The labels expose where laziness moves work and how forcing boundaries affect fusion.

## Build and run

```bash
cd ~/haskell/circuits-meter
cabal build all
cabal run circuits-meter-axioma

cd ~/haskell/circuits-examples
cabal run example-words

cd ~/machina/circuits-meter-machina
cabal run perf-explore -- sum --algorithm sumFuse --runs 1000 --warmup 1000 --nocheck
```

Use compiled optimised executables for performance claims; GHCi timing is not benchmark evidence. Allocation measurement needs `-rtsopts` at build time and `+RTS -T` at run time.
