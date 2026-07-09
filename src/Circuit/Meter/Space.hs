{-# LANGUAGE BlockArguments #-}

-- | Space measurement as a Circuit.
--
-- GHC RTS allocation statistics read before and after a computation.
-- 'allocX' measures allocated bytes; 'SpaceStats' is available for
-- users who want the full RTS snapshot.
module Circuit.Meter.Space
  ( -- * Space meter
    allocX,

    -- * Types
    SpaceStats (..),
    Bytes (..),
  )
where

import Circuit.Meter
import Control.Arrow (Kleisli (..))
import Control.Category ((.))
import Control.DeepSeq
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

-- | Number of bytes.
newtype Bytes = Bytes {unbytes :: Word64}
  deriving (Show, Read, Eq, Ord, Num, Real, Enum, Integral)

instance NFData Bytes where
  rnf (Bytes w) = rnf w

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
