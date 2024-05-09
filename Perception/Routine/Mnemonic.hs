{-# LANGUAGE LambdaCase #-}

-- | This module defines the functionality that is responsible for mnemonic
-- representation of perception routines.
module Perception.Routine.Mnemonic
  ( attachBaseWeights,
    applyPhoneticBias,
  )
where

import Numeric.Natural

-- | Attach base weights to a collection of items that can be mapped to
-- 'Char's proportionally to the frequency of those 'Char's in the English
-- language.
attachBaseWeights ::
  (Functor f) =>
  -- | The mapping function
  (a -> Char) ->
  -- | The original collection with weights
  f a ->
  -- | The resulting collection with weights assigned
  f (Natural, a)
attachBaseWeights mnemonic xs = g <$> xs
  where
    g x = (letterWeight (mnemonic x), x)

-- | Return the weight that corresponds to the given 'Char' in the English
-- language.
letterWeight :: Char -> Natural
letterWeight = \case
  -- https://en.wikipedia.org/wiki/Letter_frequency
  'e' -> 127
  't' -> 91
  'a' -> 82
  'o' -> 75
  'i' -> 70
  'n' -> 67
  's' -> 63
  'h' -> 61
  'r' -> 60
  'd' -> 43
  'l' -> 40
  'c' -> 28
  'u' -> 28
  'm' -> 24
  'w' -> 24
  'f' -> 22
  'g' -> 20
  'y' -> 20
  'p' -> 19
  'b' -> 15
  'v' -> 10
  'k' -> 8
  'j' -> 2
  'x' -> 2
  'q' -> 1
  'z' -> 1
  x -> error $ "Cannot use '" ++ [x] ++ "' as mnemonic"

-- | Classification of letters into vowels and consonants.
data Class = Vowel | Consonant
  deriving (Eq, Show)

-- | Classify a 'Char' as belonging to a 'Class'.
classify :: Char -> Class
classify x = if x `elem` "aeiouy" then Vowel else Consonant

-- | Adjust the weights in a collection of elements that can be mapped to
-- 'Char's in such a way as to encourage production of quasi-English
-- syllables.
applyPhoneticBias ::
  (Functor f) =>
  -- | The mapping function
  (a -> Char) ->
  -- | The preceding element, if any
  Maybe a ->
  -- | The original (unadjusted) collection
  f (Natural, a) ->
  -- | The resulting adjusted collection
  f (Natural, a)
applyPhoneticBias _ Nothing xs = xs
applyPhoneticBias mnemonic (Just p) xs = g <$> xs
  where
    g (n, a) =
      if classify (mnemonic a) == classify (mnemonic p)
        then (n, a)
        else (n * 2, a)
