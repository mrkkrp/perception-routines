module Perception.Routine.Probability
  ( attachEqualWeights,
    weightedSample,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Numeric.Natural
import System.Random.SplitMix

attachEqualWeights :: (Functor f) => f a -> f (Natural, a)
attachEqualWeights = fmap (1,)

weightedSample :: SMGen -> NonEmpty (Natural, a) -> (a, SMGen)
weightedSample g xs0 = (go (NonEmpty.toList xs0) i0, g')
  where
    n = max 1 (sum (fmap fst xs0))
    (w64, g') = nextWord64 g
    i0 = fromIntegral w64 `rem` n
    go [] _ = snd (NonEmpty.head xs0)
    go ((w, x) : xs) i =
      if i < w
        then x
        else go xs (i - w)
