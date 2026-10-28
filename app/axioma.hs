{-# LANGUAGE BlockArguments #-}

-- | Oracle: sanity checks for 'Circuit.Meter'.
module Main where

import Circuit.Category (K (..), (.>))
import Circuit.Meter (Meter (..), both, meterAction)
import Circuit.Meter.Stopwatch (Watches)
import Circuit.Meter.Stopwatch qualified as Stopwatch
import Circuit.Meter.Time (reifyC, ticks, timeX)
import Control.Exception (evaluate)
import Data.IORef
import System.Exit (exitFailure, exitSuccess)
import Prelude hiding (id, (.))

mkCountingMeter :: IO (Meter (K IO) Int Int, IORef Int)
mkCountingMeter = do
  ref <- newIORef 0
  pure
    ( Meter
        { start = K \_ -> atomicModifyIORef' ref (\n -> (n + 1, n + 1)),
          stop = K \prev -> atomicModifyIORef' ref (\n -> (n + 1, prev))
        },
      ref
    )

v1 :: IO ()
v1 = do
  (cnt, ref) <- mkCountingMeter
  let f = K (\x -> pure (x + 1 :: Int))
      metered = reifyC (meterAction cnt f)
  _ <- runK metered 5
  n <- readIORef ref
  putStrLn "V1: meterAction fires start + stop exactly once"
  if n == 2
    then putStrLn "  PASS"
    else do
      putStrLn $ "  FAIL: counter = " ++ show n ++ ", expected 2"
      exitFailure

v2 :: IO ()
v2 = do
  (cnt1, ref1) <- mkCountingMeter
  (cnt2, ref2) <- mkCountingMeter
  let m = both cnt1 cnt2
      f = K (\(x, y) -> pure (x + 1 :: Int, y * 2 :: Int))
      metered = reifyC (meterAction m f)
  _ <- runK metered (5, 10)
  n1 <- readIORef ref1
  n2 <- readIORef ref2
  putStrLn "V2: both(m1, m2) fires both meters"
  if n1 == 2 && n2 == 2
    then putStrLn "  PASS"
    else do
      putStrLn $ "  FAIL: counters = (" ++ show n1 ++ ", " ++ show n2 ++ "), expected (2, 2)"
      exitFailure

v3 :: IO ()
v3 = do
  t0 <- runK (start timeX) ()
  _ <- evaluate (sum [1 .. 10000 :: Int])
  dt <- runK (stop timeX) t0
  putStrLn "V3: timeX measures positive elapsed time"
  if dt > 0
    then putStrLn $ "  PASS: dt = " ++ show dt ++ "ns"
    else do
      putStrLn "  FAIL: dt = 0"
      exitFailure

v4 :: IO ()
v4 = do
  (ts, _) <- ticks 10 (\(x :: Int) -> x + 1) 42
  let allNonNeg = all (>= 0) ts
  putStrLn "V4: ticks produces non-negative measurements"
  if allNonNeg
    then putStrLn $ "  PASS: " ++ show (length ts) ++ " ticks, min = " ++ show (minimum ts)
    else do
      putStrLn "  FAIL: negative measurement"
      exitFailure

tickMeter :: IORef Int -> Meter (K IO) Int Int
tickMeter ref =
  Meter
    { start = K \_ -> readIORef ref,
      stop = K \origin -> subtract origin <$> readIORef ref
    }

gapMeter :: IORef Int -> Meter (K IO) Int Int
gapMeter ref =
  Meter
    { start = K \_ -> readIORef ref,
      stop = K \origin -> do
        now <- readIORef ref
        writeIORef ref (now + 1)
        pure (now - origin)
    }

advance :: IORef Int -> Int -> K IO () ()
advance ref n = K \_ -> modifyIORef' ref (+ n)

checkLaps :: String -> Watches Int Int -> [(String, [Int])] -> IO ()
checkLaps name ws expected = do
  let actual = fmap (\(label, _) -> (label, Stopwatch.watchLaps ws label)) expected
  putStrLn name
  mapM_ (\(label, values) -> putStrLn $ "  " ++ label ++ "=" ++ show values) actual
  if actual == expected
    then putStrLn "  PASS"
    else do
      putStrLn $ "  FAIL: expected " ++ show expected
      exitFailure

v5 :: IO ()
v5 = do
  ref <- newIORef 0
  let m = tickMeter ref
      pipeline =
        Stopwatch.start m "total"
          .> Stopwatch.carry (advance ref 7)
          .> Stopwatch.lap m "A"
          .> Stopwatch.carry (advance ref 5)
          .> Stopwatch.lap m "B"
          .> Stopwatch.carry (advance ref 3)
          .> Stopwatch.stop m "total"
  (_, ws) <- runK (reifyC pipeline) ()
  checkLaps "V5: stop records the enclosing interval" ws [("A", [7]), ("B", [5]), ("total", [15])]

v6 :: IO ()
v6 = do
  ref <- newIORef 0
  let m = tickMeter ref
      pipeline =
        Stopwatch.start m "total"
          .> Stopwatch.carry (advance ref 11)
          .> Stopwatch.stop m "total"
  (_, ws) <- runK (reifyC pipeline) ()
  checkLaps "V6: no-lap stop records the enclosing interval" ws [("total", [11])]

v7 :: IO ()
v7 = do
  ref <- newIORef 0
  let m = gapMeter ref
      pipeline =
        Stopwatch.start m "total"
          .> Stopwatch.carry (advance ref 7)
          .> Stopwatch.lap m "A"
          .> Stopwatch.stop m "total"
  (_, ws) <- runK (reifyC pipeline) ()
  checkLaps "V7: one-tick lap restart gap is enclosed, not folded from laps" ws [("A", [7]), ("total", [8])]

v8 :: IO ()
v8 = do
  ref <- newIORef 0
  let m = tickMeter ref
      pipeline =
        Stopwatch.start m "total"
          .> Stopwatch.carry (advance ref 4)
          .> Stopwatch.lap m "A"
          .> Stopwatch.carry (advance ref 3)
          .> Stopwatch.stop m "total"
          .> Stopwatch.carry (advance ref 2)
          .> Stopwatch.lap m "after"
  (_, ws) <- runK (reifyC pipeline) ()
  checkLaps "V8: post-stop lap continues the segment origin retained by stop" ws [("A", [4]), ("total", [7]), ("after", [5])]

main :: IO ()
main = do
  putStrLn "circuits-meter-verify"
  putStrLn ""
  v1
  v2
  v3
  v4
  v5
  v6
  v7
  v8
  putStrLn ""
  putStrLn "All verify assertions passed."
  exitSuccess
