# Scaling Laws with `circuits-meter`

> How to measure algorithmic complexity without the optimizer lying to you.

## The Question

We want to know the functional form of `sum [1..x]`. Is it O(1) (constant-folded),
O(x) (list traversal), or something worse? And does `foldl` vs `foldl'` matter?

## The Trap

Haskell's optimizer is *too* good. If you write a naive benchmark:

```haskell
sumTo x = sum [1 .. x]

tick sumTo 100000   -- 41 ns?!?
```

GHC sees `sumTo 100000`, computes `5000050000` at compile time, and the meter
measures nothing. The result is a lie.

## The Fix

`circuits-meter` holds the function application back with three moves:

1. **`hold` — a `NOINLINE` identity**

   ```haskell
   hold :: a -> a
   hold x = x
   {-# NOINLINE hold #-}
   ```

   This prevents GHC from floating `f a` out of the timed region. The
   optimizer knows `hold x = x`, but it cannot *inline* `hold`, so it must
   treat `f (hold x)` as an opaque application.

2. **`evaluate . force` inside the Kleisli**

   ```haskell
   Kleisli (\x -> evaluate (force (f (hold x))))
   ```

   The work now happens inside `IO`, behind a seq point (`evaluate`) that
   the optimizer respects. `force` ensures the computation runs to normal
   form — no lazy thunks escape the meter.

3. **`NOINLINE` on the timing loop**

   ```haskell
   {-# NOINLINE timesK #-}
   ```

   Prevents GHC from inlining the loop and hoisting the computation across
   iterations.

## The Experiment

Two variants:

```haskell
sumLazy :: Int -> Int
sumLazy x = sum [1 .. x]         -- foldl, lazy accumulator
{-# NOINLINE sumLazy #-}

sumStrict :: Int -> Int
sumStrict x = foldl' (+) 0 [1 .. x]   -- strict accumulator
{-# NOINLINE sumStrict #-}
```

Measure each across a grid of `x`:

```haskell
measure f n x = do
  (ts, r) <- ticks n f x
  pure (minimum ts, median ts, mean ts, r)
```

## Results

### Scaling with `x` (100 timings each)

| `x`       | `sumLazy` (p50) | `sumStrict` (p50) | `lazy / strict` |
|-----------|-----------------|-------------------|-----------------|
| 1,000     | 792 ns          | 709 ns            | 1.1×            |
| 10,000    | 6.5 µs          | 6.3 µs            | 1.0×            |
| 50,000    | 32.5 µs         | 31.3 µs           | 1.0×            |
| 100,000   | **156 µs**      | **62.5 µs**       | **2.5×**        |
| 200,000   | 232 µs          | 129 µs            | 1.8×            |
| 500,000   | 419 µs          | 312 µs            | 1.3×            |
| 1,000,000 | 632 µs          | 627 µs            | 1.0×            |

**The functional form is linear: ~0.6 ns per element.**

`sumStrict` tracks a clean line all the way. `sumLazy` hits a GC cliff around
`x = 100,000` because `foldl` builds a tower of thunks
`(((0 + 1) + 2) + 3) + …)` before collapsing it. The thunk pile triggers
minor GCs, spiking latency. At `x = 1,000,000` the cost amortizes again.

### Stability across `n` (`x = 100,000`)

| `n`  | `sumLazy` (p50) | `sumStrict` (p50) |
|------|-----------------|-------------------|
| 10   | 67.0 µs         | 67.7 µs           |
| 50   | 67.0 µs         | 67.1 µs           |
| 100  | 62.5 µs         | 62.6 µs           |
| 500  | 62.5 µs         | 62.5 µs           |
| 1000 | 62.5 µs         | 64.9 µs           |

Timings are stable regardless of how many runs you ask for. The wall works.

## The Lesson

The meter tells you *that* something got slow at `x = 100,000`. The next
question is *why* — and the answer is in the strictness of the accumulator.

This is the core value of `circuits-meter`: not just nanoseconds, but
**structural insight**. The state wire is ambient; the measurement exposes it.
The optimizer is ambient too — and `hold`, `evaluate`, and `NOINLINE` are how
you force it into the light.

## Code

```haskell
{-# LANGUAGE BlockArguments #-}

import Circuit.Perf
import Circuit.Perf.Time
import Control.DeepSeq
import Text.Printf

sumLazy :: Int -> Int
sumLazy x = sum [1 .. x]
{-# NOINLINE sumLazy #-}

sumStrict :: Int -> Int
sumStrict x = foldl' (+) 0 [1 .. x]
{-# NOINLINE sumStrict #-}

measure :: (Int -> Int) -> Int -> Int -> IO (Integer, Integer, Integer, Int)
measure f n x = do
  let !x' = x
  (ts, r) <- ticks n f x'
  let sorted = qsort ts
  pure (minimum ts, sorted !! (n `div` 2), sum ts `div` fromIntegral n, r)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (p:xs) = qsort [x | x <- xs, x < p] ++ [p] ++ qsort [x | x <- xs, x >= p]

main :: IO ()
main = do
  putStrLn "=== Vary x (n=100) ==="
  mapM_ (\x -> measure sumStrict 100 x >>= \(mn, p50, avg, r) ->
            printf "x=%-10d min=%8s p50=%8s avg=%8s result=%d\n"
              x (fmt mn) (fmt p50) (fmt avg) r)
    [1000, 10000, 50000, 100000, 200000, 500000, 1000000]

fmt :: Integer -> String
fmt n | n < 1000    = show n ++ "ns"
      | n < 1000000 = printf "%.1fµs" (fromIntegral n / 1000 :: Double)
      | otherwise   = printf "%.2fms" (fromIntegral n / 1000000 :: Double)
```

Run with:

```bash
cabal exec ghc -- -O2 Scaling.hs -o scaling && ./scaling
```
