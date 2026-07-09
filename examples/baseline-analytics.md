# circuits-meter baseline analytics

Calibration data for Haskell performance measurement using the public
`circuits-meter` API. All numbers are from an Apple M4 Pro, GHC 9.14.1,
`clang -O2`, `CLOCK_MONOTONIC_RAW`.

## Running

```bash
# Haskell baseline
cabal run baseline -- --size 100000 --count 10000000 --runs 100

# With distribution charts
cabal run baseline -- --size 100000 --count 10000000 --runs 100 --chartdir other/charts

# C references
clang -O2 -o other/c_baseline other/c_baseline.c && other/c_baseline
clang -O2 -fno-vectorize -fno-slp-vectorize -o other/c_baseline_novec other/c_baseline.c && other/c_baseline_novec
clang -O0 -o other/c_baseline_o0 other/c_baseline.c && other/c_baseline_o0
clang -O2 -o other/c_funcapp other/c_funcapp.c && other/c_funcapp
```

## Function-application floor

10 000 000 iterations, averaged over 100 runs:

| benchmark | total | per iteration | vs empty loop |
|---|---|---|---|
| `nothingN` | 6 ms | **0.70 ns** | — |
| `constUnitN` | 20 ms | **2.00 ns** | +1.30 ns |
| `bindLoopN` | 6 ms | **0.60 ns** | — |
| `pureUnitN` | 20 ms | **2.00 ns** | +1.40 ns |

A Haskell function call costs about **1.4 ns** on this machine — the *call
ceremony*, not a closure and not the monad. The Core proves both halves:

- The loop is real, not eliminated. `nothingN`'s worker is bare decrement /
  compare-zero / jump (`$wgo3 = \ww -> case ww of {_ -> $wgo3 (ww-1#); 0# -> (##)}`),
  and the per-iteration cost scales dead-linearly with `count` (5M→3ms, 10M→6ms,
  20M→13ms), so the 0.6 ns floor is real loop control, not a constant-folded no-op.
- The +1.4 ns is one `NOINLINE` call per iteration. `constUnitN` adds a call to
  `$wconstUnit = \_ -> (##)` (takes/returns unit, empty body); `pureUnitN` adds a
  call to `$wpureUnit = \s -> s` — **`IO` compiled to the identity on `State#`**.
  No heap closure is allocated (both are top-level workers, called directly), and
  the monad contributes *zero*: the pure path and the IO path pay the identical
  +1.4 ns because they bottom out at the same thing — the ceremony of an opaque
  (non-inlinable) call. What you pay for is the call, not the abstraction.

### C reference

| benchmark | per iteration | call delta |
|---|---|---|
| empty loop | 0.33 ns | — |
| `const unit` (volatile indirect call) | 0.97 ns | **0.64 ns** |

Haskell function-application overhead is roughly **2×** a C indirect function
call. The absolute cost is small: ~1.3 ns.

## List primitives

100 000 elements, averaged over 100 runs (live: `--size 100000`):

| benchmark | per element | what the loop does |
|---|---|---|
| `listLength` | **2.11 ns** | walk spine, count cells — never touches the element |
| `monoSum` | **3.53 ns** | walk spine **+ unbox `I#` + `+#`** per element |

The two loops are structurally identical except for element access — the Core makes
the decomposition exact:

```haskell
-- listLength → GHC's $wlenAcc (imported): counts cells, ignores the value
case ds of { [] -> acc ; _ : ys -> lenAcc ys (acc +# 1#) }

-- monoSum → $wgo4: same spine walk, but dereferences + unboxes each element
case ds of { [] -> ww ; y : ys -> case y of { I# y1 -> $wgo4 ys (+# ww y1) } }
```

Both: same `:` pattern-match, same pointer-chase down the spine, same tail-recursive
unboxed `Int#` accumulator, zero allocation. The **only** difference is
`case y of { I# y1 -> … }` — following the element pointer and unboxing the box.

So the per-element cost splits cleanly:

- **2.11 ns = the spine pointer-chase alone** (load next cons cell, branch, bump
  counter). This is the pure cost of the linked-list *structure*.
- **3.53 − 2.11 = 1.42 ns = reading the data** (deref element pointer, unbox `I#`,
  `+#`).

**The structure costs more than the work.** For a `sum`, over half the time goes to
chasing pointers down the spine before any arithmetic happens. GHC produced the
textbook-perfect loop (unboxed accumulator, one unbox per element, primitive add,
tail call) — the 3.5 ns is not the compiler failing, it is the boxed-cons-cell
data layout.

> Caveat: `[1 .. n]` allocates cons cells in order, so this is a **best-case**
> spine — roughly contiguous in memory. A fragmented list (post-GC, interleaved
> allocation) chases further and costs more. This is the same pointer-chase
> mechanism the cache-staircase tier will stress deliberately; here it is unstressed.

## C sum calibration

100 000 `int`s, averaged over 100 runs:

| C variant | per element | vs Haskell `monoSum` |
|---|---|---|
| `-O2` vectorized | **0.099 ns** | ~42× faster |
| `-O2`, no vectorization | **0.905 ns** | ~4.6× faster |
| `-O0` (naive) | **2.428 ns** | **~1.7× slower** |

The huge vectorized gap is SIMD, not Haskell. Against scalar-optimized C the
Haskell list sum is only ~4.6× slower; against naive C it is actually faster.
The remaining gap is the linked-list data structure.

## Core for the tight sum loop

`examples/SumCore.hs` contains isolated sum / length functions. Dumping
`-ddump-simpl` shows GHC produces exactly one worker loop shared by `monoSum`
and `sumExplicit`:

```haskell
$wgo1_r16q
  :: [Int] -> GHC.Internal.Prim.Int# -> GHC.Internal.Prim.Int#
  = \ (ds :: [Int]) (ww :: GHC.Internal.Prim.Int#) ->
      case ds of {
        [] -> ww;
        : y ys ->
          case y of { GHC.Internal.Types.I# y1 ->
          $wgo1_r16q ys (GHC.Internal.Prim.+# ww y1)
          }
      }
```

You cannot ask for a better looking loop: the accumulator is unboxed `Int#`,
each element is unboxed from `I#` once, the addition is the primitive `+#`, and
the recursion is tail-recursive. The 4 ns/element cost is not the compiler
failing — it is the list data structure.

## State-hiding (the skolem / existential fold) — two facts, kept separate

`sumHidden` and `sumMealy` route the same sum through an **existential**: the state
type is hidden behind `forall s.`, and the step/seed/extract functions ride in a
boxed record.

```haskell
data Fold  a b = forall s. Fold  (s -> a -> s) s (s -> b)
data Mealy a b = forall s. Mealy (a -> s) (s -> a -> s) (s -> b)
sumHidden xs = runFold  (Fold (+) (0 :: Int) id) xs
sumMealy  xs = runMealy (Mealy id (+) id) xs
```

Live (100k elements, three repeats, stable ordering):

| benchmark | p50 | loop accumulator | at the timing boundary |
|---|---|---|---|
| `sumExplicit` | ~348 ns/k → 3.48/elem | **unboxed `Int#`, `+#`** | inlined `$wmonoSum`, then **re-boxes** `I# ww` |
| `sumHidden` | ~330 ns/k → 3.30/elem | **boxed `Int`, dict `+`** | opaque `NOINLINE` call, returns boxed `Int` |
| `sumMealy` | ~332 ns/k → 3.32/elem | **boxed `Int`, dict `+`** | opaque `NOINLINE` call, returns boxed `Int` |

**Fact 1 — the existential is genuinely erased (the real finding).** Core shows
`runFold` scrutinizes the box exactly once and hands three fields to a specialized
worker; there is no `forall s` box *in the loop*:

```haskell
runFold … = case ds of { Fold @s ww ww1 ww2 -> $wrunFold ww ww1 ww2 xs }
$wrunFold ww ww1 ww2 xs =            -- ww, ww1, ww2 are the step/seed/extract
  ww2 (joinrec { go1 ds eta = case ds of
                   { [] -> eta ; y:ys -> case eta of z -> jump go1 ys (ww z y) } }
       jump go1 xs ww1)
```

The skolemized state costs nothing structural — the abstraction leaves no residue.
`runMealy` is identical modulo the initial `inject`.

**Fact 2 — the ~5% "faster" is a harness artifact, NOT abstraction being free.**
Do not read the table as "existential folds beat the plain one." Two boundary
effects, pulling opposite ways, produce the residual:

- the skolem loops run a **boxed `Int` accumulator with dictionary `(+)`**
  (`$fNumInt_$c+`), which is *heavier* than `monoSum`'s unboxed `+#`;
- but `sumExplicit` is **inlined and then re-boxes** its `Int#` result (`I# ww`) to
  satisfy the harness's `NFData`/`seq#` path, while `sumHidden`/`sumMealy` are
  **opaque `NOINLINE` calls** that return the boxed `Int` the harness wants directly.

The net ±5% is measurement wrapping, not fold structure. Two separate claims:
*the existential is erased* (true, Core-proven) and *abstraction is faster* (not
supported — it's a boundary artifact).

> ⚠️ Correction to an earlier draft: the three do **not** "compile to the same
> loop." `monoSum` is unboxed (`+#`); `sumHidden`/`sumMealy` are boxed (dict `+`).
> They land within ~5% *despite* different loops — the short-lived box stays hot —
> which is itself the interesting part, but "same loop" is false.


## Distribution analysis with tdigest

`Baseline.hs` now keeps the raw per-run timings and computes percentiles with
`tdigest`. A spike is defined as any value more than 2× the median.

Representative run (100 runs, 100k list elements / 10M function-call iterations):

| benchmark | p50 | p90 | p99 | max | spikes |
|---|---|---|---|---|---|
| `nothingN` | 6 ms | 7 ms | 11 ms | 12 ms | 0 |
| `constUnitN` | 20 ms | 20 ms | 21 ms | 21 ms | 0 |
| `bindLoopN` | 6 ms | 6 ms | 6 ms | 7 ms | 0 |
| `pureUnitN` | 20 ms | 20 ms | 21 ms | 21 ms | 0 |
| `listLength` | 211 µs | 220 µs | 2 ms | 3 ms | **1** |
| `monoSum` | 353 µs | 356 µs | 366 µs | 366 µs | 0 |
| `polySum Int` | 331 µs | 350 µs | 574 µs | 575 µs | 0 |
| `sumExplicit` | 349 µs | 364 µs | 386 µs | 400 µs | 0 |
| `sumHidden` | 329 µs | 337 µs | 349 µs | 354 µs | 0 |
| `sumMealy` | 328 µs | 341 µs | 362 µs | 363 µs | 0 |

`listLength` shows the classic GC / scheduler hiccup: a tight p50 but a p99
almost 10× larger. Among the numeric folds, `polySum Int` has a reproducible
~1.7× p99 tail (see below) — and it is **not** a "wider type-class path": the
dictionary is hoisted out of the loop. The cause is allocation, proven next.

## The polySum tail — three instruments, three truths

`polySum Int` (a `Num a =>` fold specialized to `Int` at the call site) has a
tight p50 (~330 µs, *faster* than `monoSum`) but a stable, reproducible p99 of
~570 µs — a ~1.7× tail `monoSum` does not have. Three instruments, each finding
something the previous one could not:

**1. The timer finds the tail.** p50/p90 tight, p99 fat, across three runs — not
noise. But the timer cannot say *why*.

**2. Core explains the loop — but not the tail.** GHC does *not* specialize
`polySum`; it stays generic, but hoists the dictionary method **out of the loop**:

```haskell
polySum @a $dNum =
  let k = + $dNum ; z0 = fromInteger $dNum 0     -- dictionary read ONCE per call
  in \xs -> joinrec { go1 ds eta = case ds of
       { [] -> eta ; y:ys -> case eta of z -> jump go1 ys (k z y) } } (go1 xs z0)
```

So there is **no per-element dictionary dispatch** — `k` is an indirect call to a
hoisted closure. This kills the "wider type-class path" story: the loop cost is
nearly `monoSum`'s. Core explains the tight p50. It says nothing about the tail.

**3. The space + GC meter finds the actual cause.** `allocated_bytes` and minor-GC
counts per call (`examples/AllocProbe.hs`, `+RTS -T`):

| n | mono bytes | mono minor-GC | poly bytes | poly minor-GC |
|---|---|---|---|---|
| 10 000 | 0 | 0 | 0 | 0 |
| 100 000 | 4.13 MB | **1** | 8.29 MB | **2** |
| 1 000 000 | 70.2 MB | 17 | 87.0 MB | 21 |

At n=100k (the benchmark size) `polySum` triggers **2 minor GCs per call vs
`monoSum`'s 1** — double the stop-the-world pauses. A minor GC is exactly a
multi-µs pause; most runs dodge it (tight p50), the unlucky ones eat it (fat p99).
**That is the tail**, mechanism proven, not guessed.

> Correction of a plausible-but-wrong hypothesis: the first guess was "boxed
> accumulator allocates per element." The n=10 000 row refutes it — **zero** bytes,
> **zero** GC in both. The accumulator stays unboxed-in-registers. The extra
> allocation is that `polySum`'s specialization failure forces the `[Int]` elements
> to be materialized fully boxed for the generic consumer, ~2× the list-construction
> traffic at n=100k. Timing + Core alone would have shipped the wrong cause; only
> the space/GC meter caught it.

The lesson: **the timer says *that*, Core says *how the loop is shaped*, and only
allocation + GC counts say *why the tail exists*.** Reach for all three.

## Interpretation

- **Haskell tight loops are fine.** The Core for `monoSum` is as good as a
  scalar hand-written loop.
- **Lists are expensive.** Pointer chasing dominates for small per-element work,
  and list traversals occasionally see large latency spikes.
- **Vectorization matters.** A contiguous unboxed vector sum would close most of
  the remaining gap to C and likely remove the `listLength` tail.
- **Allocation, not the loop, drives the tails.** `polySum`'s fat p99 is extra
  minor GCs from forced list boxing — the loop itself is fine. Tails are a
  space/GC story; read them with `allocated_bytes` + GC counts, not the clock.
- **`circuits-meter` can resolve ~1 ns differences.** The `constUnitN` /
  `nothingN` delta is 1.4 ns; `monoSum` / `listLength` delta is 1.4 ns. This is
  the right granularity for measuring circuit primitives.
- **Distribution matters, not just averages.** `tdigest` exposes the occasional
  multi-millisecond spike that a simple average would hide — and a fat tail is a
  question (*why?*), not just a number.
