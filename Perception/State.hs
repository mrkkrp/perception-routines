module Perception.State
  ( State (..),
    Environment (..),
  )
where

import Numeric.Natural

data Environment = Outdoors | Indoors

data State = State
  { stEnvironment :: Environment,
    stStamina :: Natural
  }
