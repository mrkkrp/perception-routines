{-# LANGUAGE RecordWildCards #-}

-- | This module defines the state that is tracked in a perception routine.
-- This module is internal and is supposed to be only imported from
-- "Perception.Directive".
module Perception.State.Internal
  ( State (..),
    init,
    stamina,
  )
where

import Numeric.Natural
import Perception.Routine.Domain
import Prelude hiding (init)

-- | Perception routine state.
data State = State
  { -- | Environment we are in.
    stEnvironment :: Environment,
    -- | The remaining stamina. Normally, a perception routine ends when the
    -- stamina reaches zero. All perception directives decrease stamina at
    -- least by 1.
    stStamina :: Natural
  }
  deriving (Show)

-- | Initialize 'State' given a 'Domain'.
init :: Domain -> State
init (Domain stEnvironment stStamina) = State {..}

-- | Project stamina from 'State'.
stamina :: State -> Natural
stamina = stStamina
