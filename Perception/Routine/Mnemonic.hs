-- | This module defines the functionality that is responsible for mnemonic
-- representation of perception routines.
module Perception.Routine.Mnemonic
  ( assignWeights,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Numeric.Natural
import Perception.Routine.Mnemonic.LetterWeight (letterWeight)

-- | Assign weights to a collection of elements that can be mapped to
-- 'Char's in such a way as to encourage production of quasi-English words.
assignWeights ::
  -- | The mapping function
  (a -> Char) ->
  -- | The preceding 'Char's, if any
  Maybe (Maybe Char, Char) ->
  -- | The original collection
  NonEmpty a ->
  -- | The resulting weighted collection
  NonEmpty (Natural, a)
assignWeights mnemonic prev = fmap f
  where
    f x = (letterWeight prev (Just (mnemonic x)), x)
