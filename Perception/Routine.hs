module Perception.Routine
  ( Routine (..),
    make,
    render,
    explain,
  )
where

import Data.Text (Text)
import Perception.Directive (Directive)
import Perception.Directive qualified as Directive
import Perception.Routine.Mnemonic
import Perception.Routine.Probability
import Perception.State (State (..))
import System.Random.SplitMix

newtype Routine = Routine [Directive]

make :: State -> SMGen -> Routine
make st0 g0 = Routine (go st0 g0 0 Nothing id)
  where
    go st g n lastDirective acc =
      if stStamina st == 0
        then acc []
        else
          let precondition x =
                Directive.precondition x n st && Just x /= lastDirective
           in case filter precondition Directive.all of
                [] -> acc []
                xs ->
                  let (g', directive) =
                        weightedSample
                          g
                          ( applyPhoneticBias
                              Directive.mnemonic
                              lastDirective
                              (attachEqualWeights xs)
                          )
                      st' = Directive.effect directive n st
                   in go st' g' (n + 1) (Just directive) (acc . (directive :))

render :: Routine -> Text
render = undefined

explain :: Routine -> Text
explain = undefined
