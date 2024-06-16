-- | This module defines the domain of a perception routine.
module Perception.Routine.Domain
  ( Domain (..),
  )
where

import Numeric.Natural
import Test.QuickCheck

-- | The domain of a perception routine is the complete collection of
-- arguments that are needed to regenerate that routine with the exception
-- of the preudo-random generator state.
newtype Domain = Domain Natural
  deriving (Eq, Show)

instance Arbitrary Domain where
  arbitrary =
    Domain <$> (fromIntegral . getNonNegative <$> n)
    where
      n :: Gen (NonNegative Integer)
      n = arbitrary
