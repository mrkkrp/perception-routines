module Perception.State
  ( State (..),
    Environment (..),
  )
where

import Numeric.Natural

data Environment = Outdoor | Indoor

data State = State
  { stEnvironment :: Environment,
    stStamina :: Natural
  }
