{-# LANGUAGE BlockArguments #-}

-- | Exact witnesses for 'Circuit.Meter'.
module Main where

import Circuit.Category (K (..), (.>))
import Circuit.Machine (Body (..), Moore, MooreT (..), fold, foldProcess, fuse, scan, scanProcess, scanT)
import Circuit.Meter
import Circuit.Machine (Step (..))
import Circuit.Syntax (eval)
import Circuit.Tensor (Const (..), unConst)
import Control.Exception (evaluate)
import Data.IORef
import Data.Map.Strict qualified as Map
import GHC.Stats (getRTSStatsEnabled)
import System.Clock (TimeSpec (..))
import System.Exit (exitFailure)

checkEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
checkEqual name expected actual = do
  putStrLn name
  if actual == expected
    then putStrLn "  PASS"
    else do
      putStrLn $ "  FAIL: expected " ++ show expected ++ ", got " ++ show actual
      exitFailure

listMeter :: Meter IO [Int] Int [Int]
listMeter =
  Body
    (K \a -> pure [a])
    (K \(ch, a) -> pure (ch <> [a]))
    (K \ch -> pure (Const ch))

v1 :: IO ()
v1 = do
  single <- nShot 1 listMeter (41 :: Int)
  result <- nShot 4 listMeter 41
  empty <- nShot 0 listMeter 41
  negative <- nShot (-1) listMeter 41
  checkEqual
    "V1: n-shot handles the first input and empty counts"
    (Just [41], Just (replicate 4 41), Nothing, Nothing)
    (single, result, empty, negative)

v2 :: IO ()
v2 = do
  emits <- newIORef (0 :: Int)
  let meter =
        Body
          (K \a -> pure a)
          (K \(ch, a) -> pure (ch + a))
          ( K \ch -> do
              modifyIORef' emits (+ 1)
              pure (Const ch)
          )
  one <- nShot 1 meter (2 :: Int)
  multiple <- nShot 3 meter 2
  empty <- nShot 0 meter 2
  emitCount <- readIORef emits
  checkEqual "V2: n-shot emits once per non-empty run" (Just 2, Just 6, Nothing, 2) (one, multiple, empty, emitCount)

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
  before <- stamp
  after <- stamp
  checkEqual
    "V4: raw timestamps expose elapsed arithmetic and monotonic readings"
    (Nanos 200000000, True)
    (elapsed (TimeSpec 1 900000000) (TimeSpec 2 100000000), elapsed before after >= 0)

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

v5 :: IO ()
v5 = do
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
  checkEqual "V5: pipeline records labelled laps and the enclosing total" (Map.fromList [("A", [7]), ("B", [5]), ("total", [15])]) (laps ws)

v6 :: IO ()
v6 = do
  ref <- newIORef 0
  let meter = tickBracket ref
      pipeline = start meter .> carry (advance ref 11) .> stop meter "total"
  (_, ws) <- runK (eval pipeline) ()
  checkEqual "V6: a pipeline with no laps still records its total" (Map.fromList [("total", [11])]) (laps ws)

v7 :: IO ()
v7 = do
  ref <- newIORef 0
  let meter = gapBracket ref
      pipeline = start meter .> carry (advance ref 7) .> lap meter "A" .> stop meter "total"
  (_, ws) <- runK (eval pipeline) ()
  checkEqual "V7: total encloses the lap restart gap" (Map.fromList [("A", [7]), ("total", [8])]) (laps ws)

v8 :: IO ()
v8 = do
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
  checkEqual "V8: repeated labels retain chronological order" (Map.fromList [("A", [1, 2]), ("total", [3])], [1, 2]) (laps ws, lapsFor "A" ws)

v9 :: IO ()
v9 = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then putStrLn "V9: raw time+space samples skipped; rerun with +RTS -T"
    else do
      result <- nShot 3 (timeSpaceIO (evaluate . (+ 1))) (41 :: Int)
      case result of
        Just (samples, 42) ->
          checkEqual "V9: raw time+space observes every application and latest result" (3, True) (length samples, all ((>= 0) . fst) samples)
        _ -> do
          putStrLn "V9: raw time+space observes every application and latest result"
          putStrLn "  FAIL: unexpected result"
          exitFailure

v10 :: IO ()
v10 = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then putStrLn "V10: GC-forced repeated samples skipped; rerun with +RTS -T"
    else do
      space <- nShot 2 (spaceGCIO (evaluate . (+ 1))) (41 :: Int)
      both <- nShot 2 (timeSpaceGCIO (evaluate . (+ 1))) (41 :: Int)
      case (space, both) of
        (Just (spaceSamples, 42), Just (bothSamples, 42)) ->
          checkEqual "V10: GC-forced repeated meters observe every application" (2, 2, True) (length spaceSamples, length bothSamples, all ((>= 0) . fst) bothSamples)
        _ -> do
          putStrLn "V10: GC-forced repeated meters observe every application"
          putStrLn "  FAIL: unexpected result"
          exitFailure

v11 :: IO ()
v11 = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then putStrLn "V11: GC-forced pipeline samples skipped; rerun with +RTS -T"
    else do
      (_, allocWs) <- runK (eval (start allocationGC .> stop allocationGC "total")) ()
      (_, bothWs) <- runK (eval (start timeSpaceGC .> stop timeSpaceGC "total")) ()
      checkEqual "V11: GC-forced pipeline instruments record enclosing totals" (["total"], ["total"]) (Map.keys (laps allocWs), Map.keys (laps bothWs))

v12 :: IO ()
v12 = do
  calls <- newIORef (0 :: Int)
  let payload () = modifyIORef' calls (+ 1)
  warmupPayload 3 payload ()
  warmupPayload 0 payload ()
  warmupPayload (-2) payload ()
  warmupClock 2
  warmupClock 0
  actual <- readIORef calls
  checkEqual "V12: warmup kinds stay separate and payload counts are exact" 3 actual

-- | The smallest public machine surface, exercised on one pure sum body: the
-- visible and sealed runners, both folds, the fused tick, and prefix demand.
-- V1/V2 above remain the nShot half of the same surface.
machineSurface :: IO ()
machineSurface = do
  let body = Body id (uncurry (+)) Const :: Moore (,) Int (->) Int Int
      sealed = MooreT body
      tape = [1, 2, 3, 4] :: [Int]
  checkEqual
    "V13: empty and finite runners agree on the visible and sealed paths"
    ([], [], [], [1, 3, 6, 10], [1, 3, 6, 10], [1, 3, 6, 10])
    (scanProcess body [], scan sealed [], scanT sealed [], scanProcess body tape, scan sealed tape, scanT sealed tape)
  checkEqual
    "V13: folds keep the final post-commit value and nothing on an empty tape"
    (Nothing, Just 10, Nothing, Just 10)
    (foldProcess body [], foldProcess body tape, fold sealed [], fold sealed tape)
  checkEqual
    "V13: a three-element prefix of a longer tape demands only its prefix"
    [1, 3, 6]
    (take 3 (scanProcess body (1 : 2 : 3 : error "tape over-demand")))
  let Step f = fuse body
  checkEqual
    "V13: the fused tick commits and emits in one arrow"
    7
    (unConst (f (3, 4)))

main :: IO ()
main = do
  putStrLn "circuits-meter-axioma"
  mapM_ (\v -> v >> putStrLn "") [v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, machineSurface]
  putStrLn "All 13 meter witnesses passed."
