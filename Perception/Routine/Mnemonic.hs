{-# LANGUAGE LambdaCase #-}

-- | This module defines the functionality that is responsible for mnemonic
-- representation of perception routines.
module Perception.Routine.Mnemonic
  ( attachBaseWeights,
    applyPhoneticBias,
    breakIntoWords,
  )
where

import Data.Bifunctor (first)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Ratio
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
  -- | The preceding elements, if any
  Maybe (a, Maybe a) ->
  -- | The original (unadjusted) collection
  f (Natural, a) ->
  -- | The resulting adjusted collection
  f (Natural, a)
applyPhoneticBias _ Nothing xs = xs
applyPhoneticBias mnemonic (Just (p1, mp2)) xs = g <$> xs
  where
    g (n, a) =
      let discourageConsequentVowels x@(n', a') =
            case mp2 of
              Nothing -> x
              Just p2 ->
                if classify (mnemonic p1) == Vowel
                  && classify (mnemonic p2) == Vowel
                  then
                    if classify (mnemonic a') == Vowel
                      then x
                      else (n' * 2, a')
                  else x
       in discourageConsequentVowels $
            if classify (mnemonic a) == classify (mnemonic p1)
              then (n, a)
              else (n * 2, a)

-- | Break a continuous 'String' into groups of characters that are
-- word-like by inserting spaces where necessary.
breakIntoWords :: String -> String
breakIntoWords = go noHistory id
  where
    go history acc = \case
      [] -> (acc [])
      (x : xs) ->
        if charProbability x history <= wordSeparationThreshold
          && isJust history
          then go noHistory (acc . (' ' :) . (x :)) xs
          else go (addToHistory history x) (acc . (x :)) xs
    noHistory = Nothing
    addToHistory Nothing x = Just (x, Nothing)
    addToHistory (Just (h, _)) x = Just (x, Just h)

wordSeparationThreshold :: Ratio Natural
wordSeparationThreshold = 1 % 20

charProbability :: Char -> Maybe (Char, Maybe Char) -> Ratio Natural
charProbability ch history =
  maybe
    wordSeparationThreshold
    fst
    (find (\(_, x) -> ch == x) normalizedLetterWeights)
  where
    weightedLetters =
      applyPhoneticBias id history (attachBaseWeights id allLetters)
    totalWeight = sum (fst <$> weightedLetters)
    normalizedLetterWeights =
      first (% totalWeight) <$> weightedLetters

allLetters :: [Char]
allLetters = ['a' .. 'z']
