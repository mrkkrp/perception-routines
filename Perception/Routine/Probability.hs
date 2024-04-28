module Perception.Routine.Probability
  ( attachEqualWeights,
    weightedSample,
  )
where

import Data.Ratio
import Numeric.Natural
import System.Random.SplitMix

attachEqualWeights :: [a] -> [(Ratio Natural, a)]
attachEqualWeights = undefined

weightedSample :: SMGen -> [(Ratio Natural, a)] -> (a, SMGen)
weightedSample = undefined
