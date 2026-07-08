{-# LANGUAGE BangPatterns #-}

-- Allocation probe: bytes-per-element for monoSum (unboxed) vs polySum (boxed dict).
-- Confirms/refutes the tier-4 GC-tail hypothesis: does polySum allocate per element?
-- Run: cabal run alloc-probe -- +RTS -T
module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Data.List (foldl')
import Data.Word (Word32, Word64)
import GHC.Stats
import System.Mem (performMajorGC)
import Text.Printf (printf)
import Prelude hiding (sum)

monoSum :: [Int] -> Int
monoSum = foldl' (+) 0
{-# NOINLINE monoSum #-}

polySum :: (Num a) => [a] -> a
polySum = foldl' (+) 0
{-# NOINLINE polySum #-}

hold :: a -> a
hold x = x
{-# NOINLINE hold #-}

mkList :: Int -> [Int]
mkList n = [1 .. n]
{-# NOINLINE mkList #-}

-- allocated_bytes delta across one forced evaluation of (f xs)
-- allocated_bytes + GC-count delta across one forced evaluation of (f xs)
allocOf :: (Int -> IO Int) -> Int -> IO (Word64, Word32)
allocOf run n = do
  _ <- run n -- warm
  performMajorGC
  s0 <- getRTSStats
  _ <- run n
  s1 <- getRTSStats
  pure (allocated_bytes s1 - allocated_bytes s0, gcs s1 - gcs s0)

runMono :: Int -> IO Int
runMono n = evaluate (force (monoSum (hold (mkList n))))

runPoly :: Int -> IO Int
runPoly n = evaluate (force ((polySum (hold (mkList n)) :: Int)))

main :: IO ()
main = do
  enabled <- getRTSStatsEnabled
  if not enabled
    then putStrLn "need +RTS -T (run: cabal run alloc-probe -- +RTS -T)"
    else do
      let sizes = [10000, 100000, 1000000]
      putStrLn "bytes allocated + minor-GC count per call (list construction + fold):"
      printf "%-10s %13s %6s %13s %6s\n" "n" "mono(B)" "mGC" "poly(B)" "mGC"
      forM_ sizes $ \n -> do
        (bm, gm) <- allocOf runMono n
        (bp, gp) <- allocOf runPoly n
        printf "%-10d %13d %6d %13d %6d\n" n bm gm bp gp
