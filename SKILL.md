# circuits-meter — agent field guide

Performance measurement for circuits. A benchmark binary (`perf-bench`)
and a `Circuit.Perf` library providing clock primitives and loop helpers.

## module map

```
Circuit.Perf      — Nanos, nanos, once, once_, times, times_, warmup
app/Main.hs       — perf-bench executable: four benchmarks + Core dump
examples/         — scaling.md, nub.md, seismo.md, core-analysis.md
```

## skill stack

`circuits-meter` is designed to be used with the `read-ghc-core` skill (user scope). The meter tells you *that* something costs 18ns; Core tells you *why*. See `examples/core-analysis.md` for the integration workflow.

Minimal. No Measure, StepMeasure, PerfT, or reporting machinery — just
clock reads and tight loops. The library is designed to be composed as a
Circuit plugin layer (see `circuits/examples/perf.md` for the design).

## build and test

```bash
cd ~/haskell/circuits-meter

# Build
cabal build

# Run benchmarks (50k iterations, 500 warmup)
cabal run perf-bench -- --runs 50000 --warmup 500

# Build with optimisations
cabal run perf-bench --ghc-options=-O2 -- --runs 50000
```

## the perf-explore pattern

The critical workflow for performance work:

1. **Compile, don't interpret.** Repl timing is worthless — GHCi is
   10-100x slower and the compilation strategy is different. Always
   `cabal build` or `cabal run` with `-O2`.

2. **Tight loop.** Measure one thing, many times. The `measureIO` pattern:
   read clock, run action, force result to NF, read clock, return delta.
   No allocation in the measurement itself (use strict binds, `INLINE`).

3. **Warmup.** First few measurements are cold (L2 miss, JIT, GC
   nursery). `warmup 500-1000` before timing.

4. **Percentiles, not averages.** Report p10/p50/p90. Averages are
   skewed by GC pauses and OS scheduling. The p50 tells you what the
   hot path actually costs.

5. **RTS options.** Build with `-rtsopts` and run with `+RTS -s` for
   allocation/GC stats. Profile with `-p -hc -l`.

## the three benchmarks

`perf-bench` measures:

1. **clock overhead** — raw MonotonicRaw resolution. Two back-to-back
   `getTime` calls. On Apple Silicon M-series: ~125ns. This is the
   noise floor — you can't measure anything faster.

2. **whileM_** — IORef counter loop. `newIORef 0`, loop reading and
   incrementing until target. Control group. ~8ns per iteration.

3. **trace-delim** — delimited continuation trace. `Trace (Kleisli IO) Either`
   counting loop via `prompt#`/`control0#`. ~18ns per iteration.

The delta (~10ns per iteration) is the cost of the delimited continuation
primitives: prompt tag allocation, continuation capture, and trampoline
dispatch.

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

For the full walkthrough — reading Core for `countIORef`, `runTrace`,
`timesK`, `reify`, and `hold` — see `examples/core-analysis.md`. That
card extracts real Core snippets and explains what each pattern means
for the meter reading.

### quick checklist

**Worker/wrapper.** GHC creates unboxed workers (prefixed `$w`) that
operate on `Int#`, `State# RealWorld` — no heap allocation for arithmetic.
If you see `I#` boxing in a hot loop, something is wrong.

**Primops.** `prompt#`, `control0#`, `newPromptTag#` are the delimited
continuation primitives. They appear directly in Core — no library
abstraction survives compilation.

**Letrec loops.** `joinrec { $wgo = \... -> ... jump $wgo ... }` is a
tail-recursive loop compiled to a jump, not a call. Good.

**INLINE/NOINLINE.** `NOINLINE` on benchmark entry points (`runTrace`,
`countIORef`) prevents GHC from inlining the entire benchmark into
the measurement loop and dead-code eliminating the work. Without it,
GHC can constant-fold a 1000-iteration counting loop to `pure 1000`.

**`hold` in Core.** Search for `hold` in the dump. It appears as
`f (hold x)` inside a `let` binding — the optimizer cannot float it
out because `hold` is `NOINLINE`. This is the anti-optimization wall.

## dependencies

circuits-meter depends on `circuits` for the `Trace` class. The
`cabal.project` references circuits as a `source-repository-package`
from GitHub. For local development, `cabal.project.local` overrides
with a relative path.

```
# cabal.project (committed, for CI)
packages: circuits-meter.cabal
source-repository-package
  type: git
  location: https://github.com/tonyday567/circuits.git
  branch: main

# cabal.project.local (not committed, for local dev)
packages: circuits-meter.cabal, ../circuits/circuits.cabal
```

## results (Apple Silicon M3, GHC 9.14.1, -O2)

| benchmark    | per-iteration | ratio |
|-------------|---------------|-------|
| clock overhead | 125ns      | 1x   |
| whileM_ (IORef) | 8ns       | baseline |
| trace-delim     | 18ns      | 2.25x |

1000 iterations of counting: whileM_ = 8µs, trace-delim = 18µs.
The delimited continuation overhead is ~10ns per step — the cost of
`prompt#` + `control0#` + closure capture.

This is fast. For comparison, a syscall is ~200-300ns, a Haskell
`forkIO` is ~1-2µs, and a `prompt#`/`control0#` pair is ~10ns.
Delimited continuations in GHC are a handful of instructions plus
a heap allocation for the continuation closure.

## gotchas

### repl timing is meaningless

`cabal repl` timing is 10-100x slower than compiled code and the
compilation strategy (bytecode vs native) is different. Always use
`cabal run` or a compiled binary for benchmarks.

### NOINLINE prevents dead code elimination

Without `NOINLINE` on the function being measured, GHC can inline the
entire benchmark loop, constant-fold it, and measure nothing. The
compiler is smart enough to compute `sum [1..1000]` at compile time.

### clock resolution

`MonotonicRaw` on macOS has ~42ns resolution but `getTime` takes ~60ns
per call (user→kernel→user transition). Two calls = ~125ns. Don't try
to measure anything below ~200ns per iteration — the noise floor is
too high.

### GC interference

GC pauses skew averages. Use p50 (median) not mean. For critical
measurements, run with `+RTS -A64M` (large allocation area) to
minimise GC frequency, or `+RTS -I0` (disable idle GC).

### O2 matters

`-O1` is the cabal default. `-O2` enables more aggressive inlining
and specialisation. For tight loops, the difference can be 2-5x.
Always benchmark with `-O2`.
