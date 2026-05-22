{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Space measurement as a Circuit.
--
-- GHC RTS statistics read before and after a computation. The meter
-- pattern matches 'Circuit.Perf.Time' exactly: 'pre' snapshots the
-- heap, 'post' diffs against the snapshot.
module Circuit.Perf.Space
  ( -- * Space meter
    spaceM,
    allocM,

    -- * Types
    SpaceStats (..),
    Bytes (..),

    -- * Labels
    spaceLabels,
  )
where

import Circuit.Perf
import Control.Category ((.))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Word (Word32, Word64)
import GHC.Stats
import System.Mem
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

diffSpace :: SpaceStats -> SpaceStats -> SpaceStats
diffSpace (SpaceStats a1 c1 m1 g1 g1') (SpaceStats a2 c2 m2 g2 g2') =
  SpaceStats (a2 - a1) (c2 - c1) m2 (g2 - g1) (g2' - g1')

getSpace :: RTSStats -> SpaceStats
getSpace s =
  SpaceStats
    { allocated = allocated_bytes s,
      copied = copied_bytes s,
      maxmem = max_mem_in_use_bytes s,
      minorgcs = gcs s,
      majorgcs = major_gcs s
    }

-- | Human-readable labels for 'SpaceStats' fields.
spaceLabels :: [Text]
spaceLabels = ["allocated", "copied", "maxmem", "minorgcs", "majorgcs"]

-- | Number of bytes.
newtype Bytes = Bytes {unbytes :: Word64}
  deriving (Show, Read, Eq, Ord, Num, Real, Enum, Integral)

instance Semigroup Bytes where
  (<>) = (+)

instance Monoid Bytes where
  mempty = 0

-- | Measure all 'SpaceStats' between pre and post.
spaceM :: Meter SpaceStats SpaceStats
spaceM =
  Meter
    { pre = getSpace <$> getRTSStats,
      post = \s -> do
        s' <- getSpace <$> getRTSStats
        pure (diffSpace s s')
    }
{-# INLINEABLE spaceM #-}

-- | Measure only allocated bytes.
allocM :: Meter Bytes Bytes
allocM =
  Meter
    { pre = fmap (Bytes . allocated_bytes) getRTSStats,
      post = \s -> do
        s' <- fmap (Bytes . allocated_bytes) getRTSStats
        pure (s' - s)
    }
{-# INLINEABLE allocM #-}
