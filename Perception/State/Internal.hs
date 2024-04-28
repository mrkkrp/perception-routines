{-# LANGUAGE RecordWildCards #-}

module Perception.State.Internal
  ( State (..),
    init,
    stamina,
  )
where

import Numeric.Natural
import Perception.Routine.Domain
import Prelude hiding (init)

data State = State
  { stEnvironment :: Environment,
    stStamina :: Natural
  }

init :: Domain -> State
init (Domain stEnvironment stStamina) = State {..}

stamina :: State -> Natural
stamina = stStamina
