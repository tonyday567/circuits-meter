# `List.nub` — the O(n²) reference shape

> When the order is `O(n²)`, the meter shows a cliff — and the least-squares
> fit lands R² ≈ 1.

This is the quadratic reference in the calibration set: a known-bad algorithm
measured live so its curve is a yardstick for spotting quadratic blowup elsewhere.

## Running

```bash
cd ~/haskell/circuits-meter
cabal run nub -- --runs 50
cabal run nub -- --runs 50 --sizes 100,200,500,1000,2000,5000,10000
```

Live harness: `examples/Nub.hs` (uses the public `Circuit.Meter.Time.ticks`).

## The subjects

```haskell
nubList = nub                      -- O(n²), classic nested scan
nubSet  = Set.toList . Set.fromList -- O(n log n), balanced tree
nubSort = map head . group . sort   -- O(n log n), sort then group
```

Input is `[1 .. n]` — all distinct, so `nub` does maximum work (every element is
compared against every kept element).

## Raw data

Apple M4 Pro, GHC 9.14.1, -O2, 50 runs per size, p50:

| `n`   | `nub` (p50) | `nubSet` (p50) | `nubSort` (p50) | `nub / nubSet` |
|-------|-------------|----------------|-----------------|----------------|
| 100   | 40.3 µs     | 1.6 µs         | 4.7 µs          | 24.8×          |
| 200   | 68.7 µs     | 6.8 µs         | 19.9 µs         | 10.1×          |
| 500   | 685.1 µs    | 9.2 µs         | 27.8 µs         | 74.7×          |
| 1000  | 1.66 ms     | 13.5 µs        | 39.0 µs         | 123.1×         |
| 2000  | 6.73 ms     | 26.2 µs        | 79.8 µs         | 256.7×         |
| 5000  | 40.89 ms    | 63.5 µs        | 210.9 µs        | 644.0×         |
| 10000 | 159.08 ms   | 129.0 µs       | 489.0 µs        | 1233.2×        |

The `nub` curve goes vertical; the O(n log n) controls stay flat. The ratio grows
linearly in n (as it must: n² / (n log n) ≈ n / log n).

## Fitting the curve

`time = c · n²`, least squares through the origin (`fitQuadratic` in the harness):

```
c = 1.594 ns   R² = 0.999933
```

| `n`   | actual    | predicted | residual |
|-------|-----------|-----------|----------|
| 100   | 40.3 µs   | 15.9 µs   | +60.5%   |
| 200   | 68.7 µs   | 63.7 µs   | +7.2%    |
| 500   | 685.1 µs  | 398.4 µs  | +41.8%   |
| 1000  | 1.66 ms   | 1.59 ms   | +3.8%    |
| 2000  | 6.73 ms   | 6.37 ms   | +5.2%    |
| 5000  | 40.89 ms  | 39.84 ms  | +2.6%    |
| 10000 | 159.08 ms | 159.36 ms | −0.2%    |

The residuals collapse to near-zero as n grows — the n² term dominates by n=10000
(−0.2%). The large small-n residuals (+60% at n=100) are honest: at small n the
constant/linear overhead (list construction, the sub-quadratic bookkeeping) hasn't
yet been swamped by n². A pure-n² model *should* over/under-shoot at the bottom.

## Note on absolute numbers

`nub` at n=10000 costs ~1.6 ns per n² pair here (~5 cycles/comparison on a ~3.5 GHz
M4 Pro core). Earlier notes on other hardware reported ~3 ns/pair — the shape
(quadratic, R²≈1) is the portable finding; the constant is machine-specific, which
is exactly why this is wired as a live exe rather than a static table.

## The lesson

The meter doesn't just say `nub` is "slow" — it gives the exact functional form.
A 100× blowup from n=1000 to n=10000, and a least-squares R² of 0.9999, means
you are looking at `O(n²)` with a real constant. That is the difference between
"feels slow" and "is quadratic at 1.6 ns/pair."

Use this curve as the reference silhouette: when some other function's meter data
fits `c·n²` this cleanly, you've found a hidden quadratic.
