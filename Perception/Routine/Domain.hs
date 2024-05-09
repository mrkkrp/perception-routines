-- | This module defines the domain of a perception routine.
module Perception.Routine.Domain
  ( Domain (..),
    Environment (..),
  )
where

import Numeric.Natural

-- | The domain of a perception routine is the complete collection of
-- arguments that are needed to regenerate that routine with the exception
-- of the preudo-random generator state.
data Domain = Domain Environment Natural
  deriving (Eq)

-- | The type of environment to generate a perception routine for.
data Environment = Outdoors | Indoors
  deriving (Eq)
