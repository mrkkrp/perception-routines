-- | This module defines the domain of a perception routine.
module Perception.Routine.Domain
  ( Domain (..),
    Environment (..),
  )
where

import Numeric.Natural
import Test.QuickCheck

-- | The domain of a perception routine is the complete collection of
-- arguments that are needed to regenerate that routine with the exception
-- of the preudo-random generator state.
data Domain = Domain Environment Natural
  deriving (Eq, Show)

instance Arbitrary Domain where
  arbitrary =
    Domain
      <$> arbitrary
      <*> (fromIntegral . getNonNegative <$> n)
    where
      n :: Gen (NonNegative Integer)
      n = arbitrary

-- | The type of environment to generate a perception routine for.
data Environment = Outdoors | Indoors
  deriving (Eq, Enum, Bounded, Show)

instance Arbitrary Environment where
  arbitrary = chooseEnum (minBound, maxBound)
