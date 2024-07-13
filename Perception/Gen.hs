{-# LANGUAGE DerivingVia #-}

-- | This module defines a generator monad that allows for weighted
-- pseudo-random sampling.
module Perception.Gen
  ( Gen,
    run,
    weighted,
  )
where

import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Numeric.Natural
import System.Random.SplitMix

-- | A monad that supports preudo-random generation of values.
newtype Gen a = Gen (SMGen -> (a, SMGen))
  deriving (Functor, Applicative, Monad) via (StateT SMGen Identity)

-- | Run the 'Gen' monad.
run :: SMGen -> Gen a -> (a, SMGen)
run g (Gen f) = f g

-- | Pick an element from a given weighted collection.
weighted :: NonEmpty (Natural, a) -> Gen a
weighted xs0 = Gen $ \g ->
  let n = max 1 (sum (fmap fst xs0))
      (w64, g') = nextWord64 g
      i0 = fromIntegral w64 `rem` n
      go [] _ = snd (NonEmpty.head xs0)
      go ((w, x) : xs) i =
        if i < w
          then x
          else go xs (i - w)
   in (go (NonEmpty.toList xs0) i0, g')
