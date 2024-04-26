module Perception.Routine
  ( Routine (..),
    make,
    render,
    explain,
  )
where

-- import Perception.Directive qualified as Directive

import Data.Text (Text)
import Perception.Directive (Directive)
import Perception.State (State)
import System.Random.SplitMix

newtype Routine = Routine [Directive]

make :: State -> SMGen -> Routine
make = undefined

render :: Routine -> Text
render = undefined

explain :: Routine -> Text
explain = undefined
