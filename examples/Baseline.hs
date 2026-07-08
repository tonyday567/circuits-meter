{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Baseline Haskell micro-benchmarks using circuits-meter.
--
-- Measures the floor of function application, the cost of polymorphism,
-- and the effect of hiding the state type in folds (the foldl / Mealy
-- existential trick).
--
-- Run with:
--   cabal run baseline -- --size 100000 --count 10000000 --runs 100
--   cabal run baseline -- --chartdir other/charts
module Main where

import Chart hiding (ticks)
import Circuit.Meter
import Circuit.Meter.Time
import Control.Arrow (Kleisli (..), runKleisli)
import Control.DeepSeq
import Control.Monad (replicateM_)
import Data.Foldable (forM_)
import Data.List qualified as List
import Data.TDigest
import Data.Text qualified as Text
import Optics.Core ((.~), (&))
import Options.Applicative
import Prettychart.Charts (hhistChart)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import Prelude hiding (sum)

-- ---------------------------------------------------------------------------
-- CLI
-- ---------------------------------------------------------------------------

data Config = Config
  { cfgSize :: !Int,
    cfgCount :: !Int,
    cfgRuns :: !Int,
    cfgChartDir :: Maybe FilePath
  }

configP :: Parser Config
configP =
  Config
    <$> option auto (long "size" <> short 'n' <> value 100000 <> help "Input list length")
    <*> option auto (long "count" <> short 'c' <> value 10000000 <> help "Function-call loop count")
    <*> option auto (long "runs" <> short 'r' <> value 100 <> help "Measurement iterations")
    <*> optional (option str (long "chartdir" <> metavar "DIR" <> help "Write distribution SVGs to DIR"))

-- ---------------------------------------------------------------------------
-- Inputs
-- ---------------------------------------------------------------------------

mkList :: Int -> [Int]
mkList n = [1 .. n]
{-# NOINLINE mkList #-}

-- ---------------------------------------------------------------------------
-- Exact function-application floor
-- ---------------------------------------------------------------------------

-- | Loop overhead: count down without doing anything.
nothingN :: Int -> ()
nothingN n = go n
  where
    go 0 = ()
    go i = go (i - 1)
{-# NOINLINE nothingN #-}

-- | Applying 'const ()' once per iteration.
constUnit :: Int -> ()
constUnit = const ()
{-# NOINLINE constUnit #-}

constUnitN :: Int -> ()
constUnitN n = go n
  where
    go 0 = ()
    go i = constUnit i `seq` go (i - 1)
{-# NOINLINE constUnitN #-}

-- | IO loop overhead: tail-recursive 'pure ()' chain without a closure.
bindLoopN :: Int -> IO ()
bindLoopN n = go n
  where
    go 0 = pure ()
    go i = go (i - 1)
{-# NOINLINE bindLoopN #-}

-- | 'pure ()' closure invoked once per iteration.
pureUnit :: IO ()
pureUnit = pure ()
{-# NOINLINE pureUnit #-}

pureUnitN :: Int -> IO ()
pureUnitN n = replicateM_ n pureUnit
{-# NOINLINE pureUnitN #-}

-- ---------------------------------------------------------------------------
-- List primitives
-- ---------------------------------------------------------------------------

listLength :: [Int] -> Int
listLength = length
{-# NOINLINE listLength #-}

-- ---------------------------------------------------------------------------
-- Polymorphism cost
-- ---------------------------------------------------------------------------

monoSum :: [Int] -> Int
monoSum = foldl' (+) 0
{-# NOINLINE monoSum #-}

polySum :: (Num a) => [a] -> a
polySum = foldl' (+) 0
{-# NOINLINE polySum #-}

-- ---------------------------------------------------------------------------
-- Existential state hiding (the foldl / Mealy trick)
-- ---------------------------------------------------------------------------

-- | foldl-style fold with hidden state type.
data Fold a b = forall s. Fold (s -> a -> s) s (s -> b)

runFold :: (NFData b) => Fold a b -> [a] -> b
runFold (Fold step seed extract) xs =
  extract (foldl' step seed xs)
{-# NOINLINE runFold #-}

-- | Mealy-style machine with hidden state type and an initial injection.
data Mealy a b = forall s. Mealy (a -> s) (s -> a -> s) (s -> b)

runMealy :: (NFData b) => Mealy a b -> [a] -> b
runMealy (Mealy inject step extract) (x : xs) =
  extract (foldl' step (inject x) xs)
runMealy _ [] = error "runMealy: empty input"
{-# NOINLINE runMealy #-}

sumExplicit :: [Int] -> Int
sumExplicit = monoSum

sumHidden :: [Int] -> Int
sumHidden xs = runFold (Fold (+) (0 :: Int) id) xs
{-# NOINLINE sumHidden #-}

sumMealy :: [Int] -> Int
sumMealy xs = runMealy (Mealy id (+) id) xs
{-# NOINLINE sumMealy #-}

-- ---------------------------------------------------------------------------
-- Reporting
-- ---------------------------------------------------------------------------

data Result = Result
  { resName :: String,
    resTimings :: [Nanos]
  }

fmt :: Nanos -> String
fmt n = let (v, u) = scaleNanos n in show (round v :: Int) <> u

scaleNanos :: Nanos -> (Double, String)
scaleNanos n
  | n < 1000 = (fromIntegral n, "ns")
  | n < 1000000 = (fromIntegral n / 1e3, "µs")
  | otherwise = (fromIntegral n / 1e6, "ms")

reportResult :: Result -> IO ()
reportResult (Result name ts) = do
  let dig :: TDigest 25
      dig = tdigest (fromIntegral <$> ts)
      p q = maybe "?" fmt (floor <$> quantile q dig :: Maybe Nanos)
      sorted = List.sort ts
      n = length ts
      p50v = sorted !! (n `div` 2)
      maxv = last sorted
      spikeThresh = 2 * fromIntegral p50v :: Double
      spikes = length [t | t <- ts, fromIntegral t > spikeThresh]
  putStrLn $
    name
      <> ": p50=" <> fmt p50v
      <> " p90=" <> p 0.9
      <> " p99=" <> p 0.99
      <> " max=" <> fmt maxv
      <> " spikes=" <> show spikes

bench :: (NFData b) => String -> Int -> ([Int] -> b) -> [Int] -> IO Result
bench name n f xs = do
  (ts, _) <- ticks n f xs
  pure (Result name ts)

benchInt :: (NFData b) => String -> Int -> (Int -> b) -> Int -> IO Result
benchInt name n f k = do
  (ts, _) <- ticks n f k
  pure (Result name ts)

benchIO :: String -> Int -> IO () -> IO Result
benchIO name n act = do
  (ts, _) <- runKleisli (timesK 0 n timeM (Kleisli (const act))) ()
  pure (Result name ts)

-- ---------------------------------------------------------------------------
-- Chart output
-- ---------------------------------------------------------------------------

chartResult :: FilePath -> Result -> IO ()
chartResult dir (Result name ts) = do
  let xs :: [Double]
      xs = fromIntegral <$> ts
      lo = minimum xs
      hi = maximum xs
      r = Range (lo - 0.05 * (hi - lo)) (hi + 0.05 * (hi - lo))
      bins = min 50 (length ts `div` 2)
      dig :: TDigest 25
      dig = tdigest xs
      p50 = maybe "?" (Text.pack . fmt . floor) (quantile 0.5 dig :: Maybe Double)
      p99 = maybe "?" (Text.pack . fmt . floor) (quantile 0.99 dig :: Maybe Double)
      title =
        Text.pack name
          <> ": p50="
          <> p50
          <> " p99="
          <> p99
      chart =
        hhistChart r bins xs
          & #hudOptions
            .~ ( defaultHudOptions
                  & #titles .~ [Priority 5 (defaultTitleOptions title & #buffer .~ 0.04)]
              )
  createDirectoryIfMissing True dir
  writeChartOptions (dir </> map cleanChar name <.> "svg") chart
  putStrLn $ "  wrote " <> name <> ".svg"
  where
    cleanChar c
      | c == ' ' = '-'
      | c == ':' = '-'
      | otherwise = c

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cfg <- execParser (info (configP <**> helper) fullDesc)
  let n = cfgSize cfg
      c = cfgCount cfg
      r = cfgRuns cfg
      xs = mkList n

  putStrLn $ "baseline: size=" <> show n <> " count=" <> show c <> " runs=" <> show r
  putStrLn ""

  putStrLn "exact function-application floor"
  r1 <- benchInt "nothingN" r nothingN c
  r2 <- benchInt "constUnitN" r constUnitN c
  r3 <- benchIO "bindLoopN" r (bindLoopN c)
  r4 <- benchIO "pureUnitN" r (pureUnitN c)
  forM_ [r1, r2, r3, r4] reportResult
  putStrLn ""

  putStrLn "list primitives"
  r5 <- bench "listLength" r listLength xs
  reportResult r5
  putStrLn ""

  putStrLn "polymorphism cost"
  r6 <- bench "monoSum" r monoSum xs
  r7 <- bench "polySum Int" r ((\x -> polySum x :: Int) . hold) xs
  forM_ [r6, r7] reportResult
  putStrLn ""

  putStrLn "state-hiding (skolem) effect"
  r8 <- bench "sumExplicit" r sumExplicit xs
  r9 <- bench "sumHidden" r sumHidden xs
  r10 <- bench "sumMealy" r sumMealy xs
  forM_ [r8, r9, r10] reportResult
  putStrLn ""

  case cfgChartDir cfg of
    Nothing -> pure ()
    Just dir -> do
      putStrLn "writing distribution charts"
      forM_ [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10] (chartResult dir)
      putStrLn ""

  putStrLn "Done."
