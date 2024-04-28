module Perception.Routine.Domain
  ( Domain (..),
    Environment (..),
  )
where

import Numeric.Natural

data Domain = Domain Environment Natural
  deriving (Eq)

data Environment = Outdoors | Indoors
  deriving (Eq)
