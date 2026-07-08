{-# LANGUAGE BangPatterns #-}

module SumCore
  ( monoSum,
    sumExplicit,
    listLength,
  )
where

import Data.List (foldl')
import Prelude hiding (sum)

monoSum :: [Int] -> Int
monoSum = foldl' (+) 0
{-# NOINLINE monoSum #-}

sumExplicit :: [Int] -> Int
sumExplicit xs = foldl' (+) 0 xs
{-# NOINLINE sumExplicit #-}

listLength :: [Int] -> Int
listLength = length
{-# NOINLINE listLength #-}
