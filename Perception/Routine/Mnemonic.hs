{-# LANGUAGE LambdaCase #-}

-- | This module defines the functionality that is responsible for mnemonic
-- representation of perception routines.
module Perception.Routine.Mnemonic
  ( WithWordBreaks (..),
    isWordConstituent,
    fromWordConstituent,
    assignWeights,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Numeric.Natural
import Perception.Routine.Mnemonic.LetterWeight (letterWeight)

-- | A wrapper around a value @a@ which indicates whether it should be
-- considered as a word constituent or a word break.
data WithWordBreaks a
  = WordConstituent a
  | WordBreak a
  deriving (Eq, Show)

-- | Is it a word constituent?
isWordConstituent :: WithWordBreaks a -> Bool
isWordConstituent = \case
  WordConstituent _ -> True
  WordBreak _ -> False

-- | Project word constituent from a 'WithWordBreaks' value. This function
-- is partial and works under the assumption that the value is indeed a word
-- constituent.
fromWordConstituent :: WithWordBreaks a -> a
fromWordConstituent = \case
  WordConstituent x -> x
  WordBreak _ -> error "Perception.Routine.Mnemonic.fromWordConstituent"

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
