{-# LANGUAGE BlockArguments #-}

-- | Exact witnesses for 'Circuit.Meter'.
module Main where

import Circuit.Category (K (..), (.>))
import Circuit.Machine (Body (..))
import Circuit.Meter
import Circuit.Syntax (eval)
import Circuit.Tensor (Const (..))
import Control.Exception (evaluate)
import Data.IORef
import Data.Map.Strict qualified as Map
import GHC.Stats (getRTSStatsEnabled)
import System.Exit (exitFailure)

checkEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
checkEqual name expected actual = do
  putStrLn name
  if actual == expected
    then putStrLn "  PASS"
    else do
      putStrLn $ "  FAIL: expected " ++ show expected ++ ", got " ++ show actual
      exitFailure

v1 :: IO ()
v1 = do
  result <- oneShot (count (+ 1)) (41 :: Int)
  checkEqual "V1: one-shot handles the first input" (1, 42) result

v2 :: IO ()
v2 = do
  single <- nShot 1 (count (+ 1)) (41 :: Int)
  result <- nShot 4 (count (+ 1)) (41 :: Int)
  empty <- nShot 0 (count (+ 1)) (41 :: Int)
  checkEqual "V2: n-shot counts every input and zero stays empty" (Just (1, 42), Just (4, 42), Nothing) (single, result, empty)

v3 :: IO ()
v3 = do
  result <- nShot 5 (time (const ())) ()
  case result of
    Just (samples, ()) ->
      checkEqual "V3: timer observes every application and keeps the latest result" (5, True) (length samples, all (>= 0) samples)
    Nothing -> do
      putStrLn "V3: timer observes every application and keeps the latest result"
      putStrLn "  FAIL: missing samples"
      exitFailure

v4 :: IO ()
v4 = do
  result <- nShot 3 (observe (\_ b -> b) (+ 1)) (41 :: Int)
  checkEqual "V4: observations are ordered and the latest result is direct" (Just ([42, 42, 42], 42)) result

v5 :: IO ()
v5 = do
  (_, initial) <- namedN 3 fst "raw" (count (const ())) () Map.empty
  (result, observations) <- namedN 2 fst "raw" (count (const ())) () initial
  (empty, unchanged) <- namedN 0 fst "raw" (count (const ())) () observations
  checkEqual
    "V5: named samples prepend in constant time and empty runs add nothing"
    (Just (2, ()), Map.fromList [("raw", [2, 3])], Nothing, observations)
    (result, observations, empty, unchanged)

v6 :: IO ()
v6 = do
  let meter = count (const ())
      done (n, _) = n >= (3 :: Int)
  converged <- converge 5 done meter (repeat ())
  exhausted <- converge 2 done meter (repeat ())
  atLimit <- converge 3 done meter (repeat ())
  immediate <- converge 1 (const True) meter [()]
  checkEqual
    "V6: convergence handles success before, at, and beyond exact fuel"
    ( "Left (3,())",
      "Right \"function did not converge after 2 iterations\"",
      "Left (3,())",
      "Left (1,())"
    )
    (show converged, show exhausted, show atLimit, show immediate)

v7 :: IO ()
v7 = do
  emits <- newIORef (0 :: Int)
  let meter =
        Body
          (K \a -> pure a)
          (K \(ch, a) -> pure (ch + a))
          ( K \ch -> do
              modifyIORef' emits (+ 1)
              pure (Const ch)
          )
  single <- oneShot meter (2 :: Int)
  one <- nShot 1 meter 2
  multiple <- nShot 3 meter 2
  empty <- nShot 0 meter 2
  emitCount <- readIORef emits
  checkEqual "V7: runners emit once per non-empty run" (2, Just 2, Just 6, Nothing, 3) (single, one, multiple, empty, emitCount)

tickBracket :: IORef Int -> Bracket IO Int Int
tickBracket ref =
  Bracket
    { before = readIORef ref,
      after = \origin -> subtract origin <$> readIORef ref
    }

gapBracket :: IORef Int -> Bracket IO Int Int
gapBracket ref =
  Bracket
    { before = readIORef ref,
      after = \origin -> do
        now <- readIORef ref
        writeIORef ref (now + 1)
        pure (now - origin)
    }

advance :: IORef Int -> Int -> K IO () ()
advance ref n = K \_ -> modifyIORef' ref (+ n)

v8 :: IO ()
v8 = do
  ref <- newIORef 0
  let meter = tickBracket ref
      pipeline =
        start meter
          .> carry (advance ref 7)
          .> lap meter "A"
          .> carry (advance ref 5)
          .> lap meter "B"
          .> carry (advance ref 3)
          .> stop meter "total"
  (_, ws) <- runK (eval pipeline) ()
  checkEqual "V8: pipeline records labelled laps and the enclosing total" (Map.fromList [("A", [7]), ("B", [5]), ("total", [15])]) (laps ws)

v9 :: IO ()
v9 = do
  ref <- newIORef 0
  let meter = tickBracket ref
      pipeline = start meter .> carry (advance ref 11) .> stop meter "total"
  (_, ws) <- runK (eval pipeline) ()
  checkEqual "V9: a pipeline with no laps still records its total" (Map.fromList [("total", [11])]) (laps ws)

v10 :: IO ()
v10 = do
  ref <- newIORef 0
  let meter = gapBracket ref
      pipeline = start meter .> carry (advance ref 7) .> lap meter "A" .> stop meter "total"
  (_, ws) <- runK (eval pipeline) ()
  checkEqual "V10: total encloses the lap restart gap" (Map.fromList [("A", [7]), ("total", [8])]) (laps ws)

v11 :: IO ()
v11 = do
  ref <- newIORef 0
  let meter = tickBracket ref
      pipeline =
        start meter
          .> carry (advance ref 1)
          .> lap meter "A"
          .> carry (advance ref 2)
          .> lap meter "A"
          .> stop meter "total"
  (_, ws) <- runK (eval pipeline) ()
  checkEqual "V11: repeated labels retain chronological order" (Map.fromList [("A", [1, 2]), ("total", [3])], [1, 2]) (laps ws, lapsFor "A" ws)

v12 :: IO ()
v12 = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then putStrLn "V12: time+space samples skipped; rerun with +RTS -T"
    else do
      result <- nShot 3 (timeSpaceIO (evaluate . (+ 1))) (41 :: Int)
      case result of
        Just (samples, 42) ->
          checkEqual "V12: time+space observes every application and the latest result" (3, True) (length samples, all ((>= 0) . fst) samples)
        _ -> do
          putStrLn "V12: time+space observes every application and the latest result"
          putStrLn "  FAIL: unexpected result"
          exitFailure

main :: IO ()
main = do
  putStrLn "circuits-meter-axioma"
  mapM_ (\v -> v >> putStrLn "") [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12]
  putStrLn "All meter witnesses passed."
