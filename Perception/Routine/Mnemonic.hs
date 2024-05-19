-- | This module defines the functionality that is responsible for mnemonic
-- representation of perception routines.
module Perception.Routine.Mnemonic
  ( assignWeights,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Numeric.Natural
import Perception.Routine.Mnemonic.LetterWeight (letterWeight)

-- | Assign weights to a collection of elements that can be mapped to
-- 'Char's in such a way as to encourage production of quasi-English words.
assignWeights ::
  -- | The mapping function
  (a -> Char) ->
  -- | The preceding 'Char', if any
  Maybe Char ->
  -- | The original collection
  NonEmpty a ->
  -- | The resulting weighted collection, also allowing for the end of word
  -- possibility ('Nothing')
  NonEmpty (Natural, Maybe a)
assignWeights mnemonic prev xs =
  NonEmpty.cons
    (letterWeight' Nothing, Nothing)
    (fmap g xs)
  where
    letterWeight' = letterWeight prev
    g x = (letterWeight' (Just (mnemonic x)), Just x)
