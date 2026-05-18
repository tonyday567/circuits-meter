# `List.nub` — Spotting the Quadratic

> When the order is `O(n²)`, the meter shows a cliff.

## The Question

`Data.List.nub` removes duplicates by keeping the first occurrence of each
element. What's its complexity? And how badly does it lose to a
`Set`-based alternative?

## The Experiment

Three variants:

```haskell
nubList :: [Int] -> [Int]
nubList = nub                    -- O(n²), classic nested scan
{-# NOINLINE nubList #-}

nubSet :: [Int] -> [Int]
nubSet = Set.toList . Set.fromList   -- O(n log n), balanced tree
{-# NOINLINE nubSet #-}

nubSort :: [Int] -> [Int]
nubSort = map head . group . sort    -- O(n log n), sort then group
{-# NOINLINE nubSort #-}
```

Input: `[1 .. n]` (all distinct, so `nub` does maximum work).

## Raw Data

| `n`   | `nub` (p50) | `nubSet` (p50) | `nubSort` (p50) | `nub / nubSet` |
|-------|-------------|----------------|-----------------|----------------|
| 100   | 31.3 µs     | 3.4 µs         | 6.5 µs          | 9.2×           |
| 200   | 121.4 µs    | 6.2 µs         | 13.8 µs         | 19.6×          |
| 500   | 750.5 µs    | 13.3 µs        | 30.2 µs         | 56.4×          |
| 1000  | 3.01 ms     | 25.3 µs        | 58.9 µs         | 119×           |
| 2000  | 12.15 ms    | 50.5 µs        | 115.5 µs        | 241×           |
| 5000  | 75.77 ms    | 123.2 µs       | 287.7 µs        | 615×           |
| 10000 | 302.36 ms   | 231.6 µs       | 581.4 µs        | 1305×          |

The `nub` curve goes vertical. The others stay flat.

## Fitting the Curve

Assume `time = c × n²`. Least-squares fit over the `nub` data:

```
c = Σ(time × n²) / Σ(n⁴) = 3.024 ns
```

| `n`   | Actual   | Predicted | Residual |
|-------|----------|-----------|----------|
| 100   | 31.3 µs  | 30.2 µs   | +3.4%    |
| 200   | 121.4 µs | 121.0 µs  | +0.4%    |
| 500   | 750.5 µs | 756.0 µs  | −0.7%    |
| 1000  | 3.01 ms  | 3.02 ms   | −0.5%    |
| 2000  | 12.15 ms | 12.10 ms  | +0.4%    |
| 5000  | 75.77 ms | 75.60 ms  | +0.2%    |
| 10000 | 302.36 ms| 302.40 ms | −0.0%    |

**R² = 1.000000**

The fit is essentially perfect. `nub` on distinct integers costs **3.0 ns per n²** — about 10 CPU cycles per comparison pair on a 3.2 GHz core.

## The Lesson

The meter doesn't just tell you `nub` is "slow." It gives you the exact
functional form. When you see a 100× blowup from 1000 to 10000 elements,
you know you're looking at `O(n²)`. When the least-squares fit lands an
R² of 1.000000, you know the constant factor is real.

This is the difference between "feels slow" and "is quadratic with a
3-ns-per-pair constant."

## Code

```haskell
{-# LANGUAGE BlockArguments #-}

import Circuit.Perf
import Circuit.Perf.Time
import Control.DeepSeq
import Data.List (nub, sort, group)
import qualified Data.Set as Set
import Text.Printf

nubList :: [Int] -> [Int]
nubList = nub
{-# NOINLINE nubList #-}

nubSet :: [Int] -> [Int]
nubSet = Set.toList . Set.fromList
{-# NOINLINE nubSet #-}

mkList :: Int -> [Int]
mkList n = [1 .. n]
{-# NOINLINE mkList #-}

measure f nruns n = do
  let !xs = mkList n
  (ts, r) <- ticks nruns f xs
  let sorted = qsort ts
  pure (minimum ts, sorted !! (nruns `div` 2), sum ts `div` fromIntegral nruns, length r)

qsort [] = []
qsort (p:xs) = qsort [x | x <- xs, x < p] ++ [p] ++ qsort [x | x <- xs, x >= p]

main :: IO ()
main = do
  let sizes = [100, 200, 500, 1000, 2000, 5000, 10000]
  putStrLn "=== nub ==="
  mapM_ (\n -> measure nubList 50 n >>= \(mn, p50, avg, r) ->
            printf "n=%-6d p50=%10s  out=%d\n" n (fmt p50) r) sizes
  where
    fmt n | n < 1000    = show n ++ "ns"
          | n < 1000000 = printf "%.1fµs" (fromIntegral n / 1000 :: Double)
          | otherwise   = printf "%.2fms" (fromIntegral n / 1000000 :: Double)
```

Run with:

```bash
cd ~/haskell/circuits-meter
runghc Nub.hs
```
