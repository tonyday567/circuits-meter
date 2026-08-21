{-# LANGUAGE BlockArguments #-}

-- | Space measurement as a Circuit.
--
-- GHC RTS allocation statistics read before and after a computation.
-- 'allocX' measures allocated bytes; 'SpaceStats' is available for
-- users who want the full RTS snapshot.
module Circuit.Meter.Space
  ( -- * Space meter
    allocX,
    allocGC,

    -- * Types
    SpaceStats (..),
    Bytes (..),
  )
where

import Circuit.Meter
import Control.Arrow (Kleisli (..))
import Control.Category ((.))
import Data.Word (Word32, Word64)
import GHC.Stats
import System.Mem (performGC)
import Prelude hiding (id, (.))

-- | Allocation statistics from the GHC RTS.
data SpaceStats = SpaceStats
  { allocated :: !Word64,
    copied :: !Word64,
    maxmem :: !Word64,
    minorgcs :: !Word32,
    majorgcs :: !Word32
  }
  deriving (Read, Show, Eq)

instance Semigroup SpaceStats where
  (<>) = addSpace

instance Monoid SpaceStats where
  mempty = SpaceStats 0 0 0 0 0

addSpace :: SpaceStats -> SpaceStats -> SpaceStats
addSpace (SpaceStats a1 c1 m1 g1 g1') (SpaceStats a2 c2 m2 g2 g2') =
  SpaceStats (a1 + a2) (c1 + c2) (max m1 m2) (g1 + g2) (g1' + g2')

-- | Number of bytes.
newtype Bytes = Bytes {unbytes :: Word64}
  deriving (Show, Read, Eq, Ord, Num, Real, Enum, Integral)

instance Semigroup Bytes where
  (<>) = (+)

instance Monoid Bytes where
  mempty = 0

-- | Measure only allocated bytes.
allocX :: Meter (Kleisli IO) Bytes Bytes
allocX =
  mkMeter
    (fmap (Bytes . allocated_bytes) getRTSStats)
    ( \s -> do
        s' <- fmap (Bytes . allocated_bytes) getRTSStats
        pure (s' - s)
    )
{-# INLINEABLE allocX #-}

-- | Measure allocated bytes with GC-forced boundaries.
--
-- Forces a major GC before reading the counter at both start and stop.
-- This gives accurate per-interval allocation at the cost of GC overhead
-- (a stop-the-world collection on every measurement boundary).
--
-- Use 'allocX' for lightweight (but potentially stale) measurements;
-- use 'allocGC' when you need accurate per-stage numbers.
allocGC :: Meter (Kleisli IO) Bytes Bytes
allocGC =
  mkMeter
    (performGC >> fmap (Bytes . allocated_bytes) getRTSStats)
    ( \s -> do
        performGC
        s' <- fmap (Bytes . allocated_bytes) getRTSStats
        pure (s' - s)
    )
{-# INLINEABLE allocGC #-}
