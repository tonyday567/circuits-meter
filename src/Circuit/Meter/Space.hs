{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Space measurement as a Circuit.
--
-- GHC RTS statistics read before and after a computation. The meter
-- pattern matches 'Circuit.Meter.Time' exactly: 'enter' snapshots the
-- heap, 'exit' diffs against the snapshot.
module Circuit.Meter.Space
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

import Circuit.Meter
import Control.Arrow (Kleisli (..))
import Control.Category ((.))
import Control.DeepSeq
import Data.Text (Text)
import Data.Word (Word32, Word64)
import GHC.Stats
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

instance NFData SpaceStats where
  rnf (SpaceStats a c m g1 g2) = rnf a `seq` rnf c `seq` rnf m `seq` rnf g1 `seq` rnf g2

instance Semigroup SpaceStats where
  (<>) = addSpace

instance Monoid SpaceStats where
  mempty = SpaceStats 0 0 0 0 0

addSpace :: SpaceStats -> SpaceStats -> SpaceStats
addSpace (SpaceStats a1 c1 m1 g1 g1') (SpaceStats a2 c2 m2 g2 g2') =
  SpaceStats (a1 + a2) (c1 + c2) (max m1 m2) (g1 + g2) (g1' + g2')

diffSpace :: SpaceStats -> SpaceStats -> SpaceStats
diffSpace (SpaceStats a1 c1 _m1 g1 g1') (SpaceStats a2 c2 m2 g2 g2') =
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

instance NFData Bytes where
  rnf (Bytes w) = rnf w

instance Semigroup Bytes where
  (<>) = (+)

instance Monoid Bytes where
  mempty = 0

-- | Measure all 'SpaceStats' between pre and post.
spaceM :: Meter (Kleisli IO) SpaceStats SpaceStats
spaceM =
  mkMeter
    (getSpace <$> getRTSStats)
    ( \s -> do
        s' <- getSpace <$> getRTSStats
        pure (diffSpace s s')
    )
{-# INLINEABLE spaceM #-}

-- | Measure only allocated bytes.
allocM :: Meter (Kleisli IO) Bytes Bytes
allocM =
  mkMeter
    (fmap (Bytes . allocated_bytes) getRTSStats)
    ( \s -> do
        s' <- fmap (Bytes . allocated_bytes) getRTSStats
        pure (s' - s)
    )
{-# INLINEABLE allocM #-}
