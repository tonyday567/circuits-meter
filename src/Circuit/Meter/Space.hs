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

import Circuit.Category (K (..))
import Circuit.Meter
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

-- | Measure the delta between two direct samples of the RTS cumulative
-- @allocated_bytes@ counter. Requires @+RTS -T@.
--
-- No GC is forced at either boundary. The delta can therefore omit recent
-- nursery allocation that is not yet visible in the RTS statistics sample;
-- it is not a guaranteed total for the metered payload.
allocX :: Meter (K IO) Bytes Bytes
allocX =
  mkMeter
    (fmap (Bytes . allocated_bytes) getRTSStats)
    ( \s -> do
        s' <- fmap (Bytes . allocated_bytes) getRTSStats
        pure (s' - s)
    )
{-# INLINEABLE allocX #-}

-- | Measure the delta between two post-GC samples of the RTS cumulative
-- @allocated_bytes@ counter. Requires @+RTS -T@.
--
-- 'performGC' runs before each sample, refreshing the statistics and adding
-- GC overhead to the measured interval. The delta is not a guaranteed total
-- for the metered payload: it can include instrumentation allocation and is
-- affected by sharing and by work completed before the interval.
allocGC :: Meter (K IO) Bytes Bytes
allocGC =
  mkMeter
    (performGC >> fmap (Bytes . allocated_bytes) getRTSStats)
    ( \s -> do
        performGC
        s' <- fmap (Bytes . allocated_bytes) getRTSStats
        pure (s' - s)
    )
{-# INLINEABLE allocGC #-}
