{-# LANGUAGE BlockArguments #-}

-- | Oracle: sanity checks for 'Circuit.Meter'.
module Main where

import Circuit.Meter (Meter (..), both, meterAction)
import Circuit.Category (K (..))
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

main :: IO ()
main = do
  putStrLn "circuits-meter-verify"
  putStrLn ""
  v1
  v2
  v3
  v4
  putStrLn ""
  putStrLn "All verify assertions passed."
  exitSuccess
