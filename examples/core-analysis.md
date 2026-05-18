# Core Analysis — From Nanoseconds to Machine Code

> The meter tells you *that* something costs 18ns. Core tells you *why*.

`circuits-meter` measures circuits. GHC Core shows what the compiler actually built. This card connects the two: we run the benchmarks, dump the `-ddump-simpl`, and read the loops that the meter is timing.

## Generating Core

```bash
cd ~/haskell/circuits-meter

cabal build perf-bench \
  --ghc-options="-ddump-simpl -ddump-to-file \
                 -dsuppress-all -dno-suppress-type-signatures \
                 -fforce-recomp -O2"
```

Output lands here:
```
dist-newstyle/build/.../perf-bench-tmp/app/Main.dump-simpl
dist-newstyle/build/.../circuits-meter-*/build/src/Circuit/Perf.dump-simpl
```

Flags:
- `-ddump-simpl` — Core after all optimizations
- `-dsuppress-all` — hide module prefixes and unique IDs (readable)
- `-dno-suppress-type-signatures` — keep types (essential for spotting boxed values)
- `-fforce-recomp` — don't let cabal skip compilation

For deeper reading, the `read-ghc-core` skill (user scope) is the definitive reference. This card is the `circuits-meter` application layer.

---

## Case Study 1: `countIORef` — The Gold Standard

From `app/Main.hs`:

```haskell
countIORef :: Int -> IO Int
countIORef target = do
  ref <- newIORef 0
  let loop = do
        n <- readIORef ref
        if n >= target
          then pure n
          else writeIORef ref (n + 1) >> loop
  loop
{-# NOINLINE countIORef #-}
```

The meter says: **8ns per iteration** (p50). Let's see the loop.

Search the dump for `$wcountIORef`:

```haskell
$wcountIORef
  :: Int# -> State# RealWorld -> (# State# RealWorld, Int #)
$wcountIORef
  = \ (ww_s6qC :: Int#) (s_s6qE :: State# RealWorld) ->
      case newMutVar# n_r6tx s_s6qE of { (# ipv_a5od, ipv1_a5oe #) ->
      joinrec {
        loop_s6dV :: State# RealWorld -> (# State# RealWorld, Int #)
        loop_s6dV (s1_X1Q :: State# RealWorld)
          = case readMutVar# ipv1_a5oe s1_X1Q of ds1_a52D
            { (# ipv2_a52E, ipv3_a52F #) ->
            case ipv3_a52F of { I# x_a6nY ->
            case >=# x_a6nY ww_s6qC of {
              __DEFAULT ->
                case writeMutVar# ipv1_a5oe (I# (+# x_a6nY 1#)) ipv2_a52E
                of s2#_a5p5
                { __DEFAULT ->
                jump loop_s6dV s2#_a5p5
                };
              1# -> ds1_a52D
            }
            }
            }; } in
      jump loop_s6dV ipv_a5od
      }
```

### What to notice

| Pattern | Reading |
|---------|---------|
| `Int#` → `State# RealWorld` → `(# State# RealWorld, Int #)` | Unboxed worker. No heap objects passed. |
| `newMutVar#` | Raw primop — GHC stripped the `IORef` newtype. |
| `readMutVar#` / `writeMutVar#` | Raw primops — no `MonadIO` dictionary, no `IO` wrapper. |
| `joinrec { loop_s6dV ... jump loop_s6dV ... }` | Tail-recursive loop compiled to a jump. No stack growth. |
| No `let` bindings inside the loop body | No heap allocation per iteration. |

This is a perfect loop. The 8ns is the cost of two `MutVar#` primops and an integer comparison. Everything else — `IO`, `IORef`, the `do` notation — compiled away.

**Verdict:** ✓ The abstraction is free. The 8ns is real machine work.

---

## Case Study 2: `runTrace` — Delimited Continuations in Core

From `app/Main.hs`:

```haskell
countTrace :: Int -> Kleisli IO (Either Int ()) (Either Int Int)
countTrace target = Kleisli \case
  Right () -> countUp 0
  Left n -> countUp n
  where
    countUp n
      | n >= target = pure (Right n)
      | otherwise = pure (Left (n + 1))

runTrace :: Int -> IO Int
runTrace n = runKleisli (trace (countTrace n)) ()
{-# NOINLINE runTrace #-}
```

The meter says: **18ns per iteration** — 10ns more than `countIORef`. Where does the delta come from?

Core, `benchBoth8` (the worker for `runTrace`):

```haskell
benchBoth8
  :: Int -> State# RealWorld -> (# State# RealWorld, Int #)
benchBoth8
  = \ (n1_a3Vf :: Int) (eta_B0 :: State# RealWorld) ->
      case newPromptTag# eta_B0 of { (# ipv_a5js, ipv1_a5jt #) ->
      prompt#
        ipv1_a5jt
        (\ (s1_a5jv :: State# RealWorld) ->
           case n1_a3Vf of { I# ww_s6pw ->
           case $wcountTrace ww_s6pw lvl3_r6tB s1_a5jv of
           { (# ipv2_a5jx, ipv3_a5jy #) ->
           case ipv3_a5jy of {
             Left a1_a5jB ->
               control0#
                 ipv1_a5jt
                 (\ (f#_a5jD
                       :: (State# RealWorld -> (# State# RealWorld, Int #))
                          -> State# RealWorld -> (# State# RealWorld, Int #))
                    (s2_a5jE :: State# RealWorld) ->
                    f#_a5jD
                      (letrec {
                         go_s6dr
                           :: Either Int () -> State# RealWorld -> (# State# RealWorld, Int #)
                         go_s6dr
                           = \ (x_a5jq :: Either Int ()) (eta1_a5jr :: State# RealWorld) ->
                               prompt#
                                 ipv1_a5jt
                                 (\ (s4_X1R :: State# RealWorld) ->
                                    case $wcountTrace ww_s6pw x_a5jq s4_X1R of
                                    { (# ipv4_X1T, ipv5_X1U #) ->
                                    case ipv5_X1U of {
                                      Left a2_X1W ->
                                        control0#
                                          ipv1_a5jt
                                          (\ (f#1_X1X ... ) (s5_X1Y :: State# RealWorld) ->
                                             f#1_X1X (go_s6dr (Left a2_X1W)) s5_X1Y)
                                          ipv4_X1T;
                                      Right c1_a5jF -> (# ipv4_X1T, c1_a5jF #)
                                    }
                                    })
                                 eta1_a5jr; } in
                       go_s6dr (Left a1_a5jB))
                      s2_a5jE)
                 ipv2_a5jx;
             Right c1_a5jF -> (# ipv2_a5jx, c1_a5jF #)
           }
           }
           })
        ipv_a5js
      }
```

### What to notice

| Pattern | Reading |
|---------|---------|
| `newPromptTag#` | Allocates a prompt tag — once per `runTrace` call, not per iteration. |
| `prompt#` / `control0#` | The delimited continuation primops. They survive compilation. |
| `letrec { go_s6dr ... }` | The iteration loop. Each step: `prompt#` → run body → `control0#` → capture continuation → re-enter with new state. |
| `f#_a5jD ... go_s6dr (Left a2_X1W)` | The captured continuation is a closure allocation. That's the ~10ns delta. |

The loop body (`$wcountTrace`) is identical to `countIORef`'s logic — compare an `Int#`, branch, return. The overhead is the `prompt#`/`control0#` pair and the continuation closure allocation.

**Verdict:** ⚠️ The 10ns delta is real — it's the cost of delimited continuation machinery. But it's only 10ns. A syscall is ~300ns. This is fast.

---

## Case Study 3: `timesK` — The Meter Loop

From `src/Circuit/Perf.hs`:

```haskell
timesK :: Int -> Meter s t -> Kleisli IO a b -> Kleisli IO a ([t], b)
timesK n m k = Kleisli \a -> do
  warmup 100
  let step !x = runKleisli (meterK m k) x
      go 1 !x acc = do (t, b) <- step x; pure (reverse (t : acc), b)
      go i !x acc = do (t, _) <- step x; go (i - 1) x (t : acc)
  go (max 1 n) a []
{-# NOINLINE timesK #-}
```

Core, `$wtimesK`:

```haskell
$wtimesK
  :: forall s t a b.
     Int
     -> Meter s t
     -> Kleisli IO a b
     -> a
     -> State# RealWorld
     -> (# State# RealWorld, [t], b #)
$wtimesK
  = \ (@s_s6aG) (@t_s6aH) (@a_s6aI) (@b_s6aJ)
      (n_s6aK :: Int)
      (m_s6aL :: Meter s_s6aG t_s6aH)
      (k_s6aM :: Kleisli IO a_s6aI b_s6aJ)
      (a1_s6aN :: a_s6aI)
      (s1_s6aO :: State# RealWorld) ->
      case $w$wloop_r6ck 100# s1_s6aO of ww_s6aq { __DEFAULT ->
      case n_s6aK of { I# y1_a66A ->
      let {
        lvl2_s67X :: Circuit (Kleisli IO) (,) a_s6aI (t_s6aH, b_s6aJ)
        lvl2_s67X = meterC m_s6aL (Lift k_s6aM) } in
      join {
        exit_X4
          :: a_s6aI -> [t_s6aH] -> State# RealWorld
          -> (# State# RealWorld, [t_s6aH], b_s6aJ #)
        exit_X4 (x_s6aA :: a_s6aI) (acc_s6aB :: [t_s6aH]) (eta_s6aC :: State# RealWorld)
          = case x_s6aA of x1_X5 { __DEFAULT ->
            case ((((reify
                       $dCategory_r6c7 $fTraceTYPETYPEKleisliTuple2 lvl2_s67X)
                    `cast` <Co:6> :: ...)
                     x1_X5)
                  `cast` <Co:4> :: ...)
                   eta_s6aC
            of
            { (# ipv_a66c, ipv1_a66d #) ->
            case ipv1_a66d of { (t1_a4Ui, b1_a4Uj) ->
            (# ipv_a66c, reverse1 (: t1_a4Ui acc_s6aB) [], b1_a4Uj #)
            }
            }
            } } in
      joinrec {
        $wgo_s6aD
          :: Int# -> a_s6aI -> [t_s6aH] -> State# RealWorld
          -> (# State# RealWorld, [t_s6aH], b_s6aJ #)
        $wgo_s6aD (ww1_s6ay :: Int#)
                  (x_s6aA :: a_s6aI)
                  (acc_s6aB :: [t_s6aH])
                  (eta_s6aC :: State# RealWorld)
          = case ww1_s6ay of ds_X3 {
              __DEFAULT ->
                case x_s6aA of x1_X5 { __DEFAULT ->
                case ((((reify
                           $dCategory_r6c7 $fTraceTYPETYPEKleisliTuple2 lvl2_s67X)
                        `cast` <Co:6> :: ...)
                         x1_X5)
                      `cast` <Co:4> :: ...)
                       eta_s6aC
                of
                { (# ipv_a66c, ipv1_a66d #) ->
                case ipv1_a66d of { (t1_a4VP, ds2_d5JS) ->
                jump $wgo_s6aD (-# ds_X3 1#) x1_X5 (: t1_a4VP acc_s6aB) ipv_a66c
                }
                }
                };
              1# -> jump exit_X4 x_s6aA acc_s6aB eta_s6aC
            }; } in
      case <=# 1# y1_a66A of {
        __DEFAULT -> jump $wgo_s6aD 1# a1_s6aN [] ww_s6aq;
        1# -> jump $wgo_s6aD y1_a66A a1_s6aN [] ww_s6aq
      }
      }
      }
```

### What to notice

| Pattern | Reading |
|---------|---------|
| `$w$wloop_r6ck 100#` | The warmup loop — 100 back-to-back `getTime` calls to warm the clock. Unboxed `Int#` counter. |
| `let { lvl2_s67X = meterC m_s6aL (Lift k_s6aM) }` | Constructs a `Circuit` value **once**, outside the loop. |
| `reify $dCategory_r6c7 $fTraceTYPETYPEKleisliTuple2 lvl2_s67X` | Calls `reify` on the `Circuit`. The dictionaries are passed explicitly. |
| `joinrec { $wgo_s6aD ... jump $wgo_s6aD ... }` | Tail-recursive loop. No stack growth across iterations. |
| `(: t1_a4VP acc_s6aB)` | List cons — one heap allocation per iteration. This is the accumulator cost. |
| `reverse1 ... []` | Reverses the list once at the end. Good. |

The `Circuit` constructor `Lift k_s6aM` is allocated once, and `meterC` builds a small `Compose` tree. `reify` traverses this tree each iteration. For a simple meter (one `pre`, one `post`), the tree is tiny — two `Compose` nodes. The cost is negligible compared to the clock reads inside `meterK`.

**Verdict:** ✓ The meter loop is well-formed. Tail recursion, minimal allocation (one cons per run), `Circuit` tree built once.

---

## Case Study 4: `reify` — The Constructor Eliminator

From `src/Circuit/Circuit.hs`:

```haskell
reify :: (Category arr, Trace arr t) => Circuit arr t a b -> arr a b
reify (Lift f) = f
reify (Compose (Knot f) g) = trace (f . untrace (reify g))
reify (Compose f g) = reify f . reify g
reify (Knot k) = trace k
```

Core:

```haskell
Rec {
reify
  :: forall {k1} {k2} (arr :: k1 -> k1 -> *) (t :: k2 -> k1 -> k1)
            (x :: k1) (y :: k1).
     (Category arr, Trace arr t) =>
     Circuit arr t x y -> arr x y
reify
  = \ (@k_a2uc) (@k1_a2ud)
      (@(arr_a2ue :: k_a2uc -> k_a2uc -> *))
      (@(t_a2uf :: k1_a2ud -> k_a2uc -> k_a2uc))
      (@(x_a2ug :: k_a2uc))
      (@(y_a2uh :: k_a2uc))
      ($dCategory_a2ui :: Category arr_a2ue)
      ($dTrace_a2uj :: Trace arr_a2ue t_a2uf)
      (ds_d37P :: Circuit arr_a2ue t_a2uf x_a2ug y_a2uh) ->
      case ds_d37P of {
        Lift f_a2ih -> f_a2ih;
        Compose @b_a2uo ds1_d384 g_a2ij ->
          case ds1_d384 of wild1_X2 {
            __DEFAULT ->
              . $dCategory_a2ui
                (reify $dCategory_a2ui $dTrace_a2uj wild1_X2)
                (reify $dCategory_a2ui $dTrace_a2uj g_a2ij);
            Knot @a_a2uq f_a2ii ->
              trace
                $dTrace_a2uj
                (. $dCategory_a2ui
                   f_a2ii
                   (untrace $dTrace_a2uj (reify $dCategory_a2ui $dTrace_a2uj g_a2ij)))
          };
        Knot @a_a2vH k2_a2im -> trace $dTrace_a2uj k2_a2im
      }
end Rec }
```

### What to notice

| Pattern | Reading |
|---------|---------|
| `case ds_d37P of { Lift f -> f; ... }` | Pattern match on the GADT. No runtime type dispatch — GADT indices are erased. |
| `Knot @a_a2uq f_a2ii` | The existential type `a` is passed at runtime (type application). But it's a type pointer, not a value. |
| `. $dCategory_a2ui (...)` | Dictionary-passing style. The `Category` dictionary (`.`) and `Trace` dictionary (`trace`/`untrace`) are explicit arguments. |

`reify` is a recursive function that walks the `Circuit` tree and produces an `arr` value. It does not allocate `Circuit` nodes — it consumes them. The cost of `reify` is proportional to the size of the `Circuit` AST.

**Crucially:** If `reify` is inlined into a context where the `Circuit` is known statically (e.g., a parser defined at top level), GHC will unfold `reify` and eliminate the `Circuit` constructors entirely. The parser becomes a plain function loop with no `Circuit` overhead.

**Verdict:** ✓ `reify` is a tree fold. `Circuit` is a compile-time data structure that becomes a run-time function. When inlined, the constructors vanish.

---

## Case Study 5: `hold` — Preventing the Optimizer from Cheating

From `src/Circuit/Perf.hs`:

```haskell
hold :: a -> a
hold x = x
{-# NOINLINE hold #-}
```

In the Core for `timesC`:

```haskell
timesC ... f_a4S5 ... a1_a4S6 ... =
  ((reify ... (meterC ... (Lift
     ((\ (x_a4Sa :: a_a5CE) (eta_B0 :: State# RealWorld) ->
         seq#
           (let { x1_s66L :: b_a5CB
                  x1_s66L = f_a4S5 (hold x_a4Sa) } in
            case rnf $dNFData_a5CF x1_s66L of { () -> x1_s66L })
           eta_B0)
      `cast` <Co:12> :: ...))))
   `cast` <Co:6> :: ...)
    a1_a4S6
```

### What to notice

| Pattern | Reading |
|---------|---------|
| `f_a4S5 (hold x_a4Sa)` | The function application is guarded by `hold`. |
| `let { x1_s66L = f_a4S5 (hold x_a4Sa) }` | GHC floated the application into a `let`, but it cannot eliminate `hold` because `hold` is `NOINLINE`. |
| `rnf $dNFData_a5CF x1_s66L` | Deep-forces the result inside the `IO` action. |
| `seq# ... eta_B0` | The `seq#` primop ensures evaluation happens before returning the state token. |

Without `hold`, GHC would see `f x` and potentially:
1. Float it out of the lambda (hoisting)
2. Constant-fold it (if `x` is known)
3. CSE it across iterations

With `hold`, the application is anchored inside the timed region. The `NOINLINE` barrier is a one-instruction identity function at runtime but a brick wall for the optimizer.

**Verdict:** ✓ `hold` is the anti-optimization wall. It costs zero at runtime but prevents GHC from hoisting work out of the meter.

---

## Symptom → Core Pattern → Fix

| Meter symptom | Core pattern | Fix |
|---------------|--------------|-----|
| 100× slower than expected | `I#` boxing/unboxing inside loop | Add strictness annotations, use `Int#` directly |
| GC spikes every N iterations | `let` allocation inside `joinrec` | Flatten state into unboxed args, avoid tuples in loop body |
| Bimodal distribution (fast/slow) | `case` on boxed `Either` with heap check | Ensure `INLINE` on small functions, use `UNPACK` |
| Constant time regardless of input | No `Rec` — loop was constant-folded | Add `NOINLINE` to the function being measured, use `hold` |
| `Circuit` overhead visible in profile | `reify` call with large `lvl2` binding | Ensure `reify` is inlined at the use site, or specialize |
| 10ns delta on `trace` vs `IORef` | `prompt#` + `control0#` + closure capture | Expected — the delimited continuation cost is real but small |

---

## Workflow

1. **Run the meter.** `cabal run perf-bench` gives you the number.
2. **Dump Core.** Add `-ddump-simpl` to the build flags.
3. **Find the loop.** Search for `Rec {` or `joinrec` near the function name.
4. **Check the signature.** Count `Int#`/`State#` vs `Int`/`[a]`/`Either`.
5. **Read the body.** Look for `let` allocations, dictionary lookups, `cast` chains.
6. **Correlate.** Does the Core explain the meter reading? If the loop allocates a tuple every iteration and the meter shows 500ns, that's your answer.
7. **Fix and redump.** Change the source, rebuild with `-fforce-recomp`, diff the Core.

---

## See Also

- `read-ghc-core` skill (user scope) — comprehensive guide to reading Core, including lazy knot analysis and monad abstraction overhead.
- `scaling.md` — `sumTo` case study: meter finds the thunk cliff, Core would show the boxed accumulator.
- `nub.md` — quadratic detection: meter finds the curve, Core would show the nested `letrec`.
- `seismo.md` — streaming per-run data: when Core shows an allocation per iteration, the seismograph shows the GC spike.
