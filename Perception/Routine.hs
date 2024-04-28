module Perception.Routine
  ( Routine (..),
    makeN,
    make,
    render,
    explain,
  )
where

import Data.Bifunctor (first)
import Data.List (unfoldr)
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural
import Perception.Directive (Directive)
import Perception.Directive qualified as Directive
import Perception.Routine.Mnemonic
import Perception.Routine.Probability
import Perception.State (State (..))
import System.Random.SplitMix

newtype Routine = Routine [Directive]

makeN :: Natural -> State -> SMGen -> [Routine]
makeN n0 st g0 = unfoldr makeOne (n0, g0)
  where
    makeOne (0, _) = Nothing
    makeOne (n, g) =
      let (routine, g') = make st g
       in Just (routine, (n - 1, g'))

maxDirectivesPerRoutine :: Natural
maxDirectivesPerRoutine = round (1.2 * sqrt n)
  where
    n :: Double
    n = fromIntegral (length Directive.all)

make :: State -> SMGen -> (Routine, SMGen)
make st0 g0 = first Routine (go st0 g0 0 Nothing id)
  where
    go st g n lastDirective acc =
      if stStamina st == 0 || n >= maxDirectivesPerRoutine
        then (acc [], g)
        else
          let precondition x =
                Directive.precondition x n st && Just x /= lastDirective
           in case filter precondition Directive.all of
                [] -> (acc [], g)
                xs ->
                  let (directive, g') =
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
render (Routine xs) = (Text.toTitle . Text.pack . fmap Directive.mnemonic) xs

explain :: Routine -> Text
explain = undefined
