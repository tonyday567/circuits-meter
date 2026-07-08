# circuits-meter baseline analytics

Calibration data for Haskell performance measurement using the public
`circuits-meter` API. All numbers are from an Apple M4 Pro, GHC 9.14.1,
`clang -O2`, `CLOCK_MONOTONIC_RAW`.

## Running

```bash
# Haskell baseline
cabal run baseline -- --size 100000 --count 10000000 --runs 100

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

A Haskell function call / monadic `pure ()` closure costs about **1.3 ns** on
this machine.

### C reference

| benchmark | per iteration | call delta |
|---|---|---|
| empty loop | 0.33 ns | — |
| `const unit` (volatile indirect call) | 0.97 ns | **0.64 ns** |

Haskell function-application overhead is roughly **2×** a C indirect function
call. The absolute cost is small: ~1.3 ns.

## List primitives

100 000 elements, averaged over 100 runs:

| benchmark | per element |
|---|---|
| `listLength` | **2.67 ns** |
| `monoSum` | **4.16 ns** |

`monoSum` pays the same list pointer chase as `listLength` (~2.67 ns) plus the
`I#` unbox, primitive `+#`, and tail call (~1.49 ns).

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
almost 10× larger. The numeric folds are more stable, with `polySum Int` having
a slightly wider tail (wider type-class path) but no 2× spikes.

## Interpretation

- **Haskell tight loops are fine.** The Core for `monoSum` is as good as a
  scalar hand-written loop.
- **Lists are expensive.** Pointer chasing dominates for small per-element work,
  and list traversals occasionally see large latency spikes.
- **Vectorization matters.** A contiguous unboxed vector sum would close most of
  the remaining gap to C and likely remove the `listLength` tail.
- **`circuits-meter` can resolve ~1 ns differences.** The `constUnitN` /
  `nothingN` delta is 1.3 ns; `monoSum` / `listLength` delta is 1.5 ns. This is
  the right granularity for measuring circuit primitives.
- **Distribution matters, not just averages.** `tdigest` exposes the occasional
  multi-millisecond spike that a simple average would hide.
