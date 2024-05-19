{-# LANGUAGE LambdaCase #-}

-- | This module provides a function for assigning weights to letters.
module Perception.Routine.Mnemonic.LetterWeight
  ( letterWeight,
  )
where

import Numeric.Natural

-- | Obtain weight for a given letter given the preceding letter.
letterWeight ::
  -- | The preceding letter, if any
  Maybe Char ->
  -- | The letter to get the weight for, or the end of word possibility if
  -- 'Nothing'
  Maybe Char ->
  -- | The weight
  Natural
letterWeight _ = \case
  -- https://en.wikipedia.org/wiki/Letter_frequency
  Just 'e' -> 127
  Just 't' -> 91
  Just 'a' -> 82
  Just 'o' -> 75
  Just 'i' -> 70
  Just 'n' -> 67
  Just 's' -> 63
  Just 'h' -> 61
  Just 'r' -> 60
  Just 'd' -> 43
  Just 'l' -> 40
  Just 'c' -> 28
  Just 'u' -> 28
  Just 'm' -> 24
  Just 'w' -> 24
  Just 'f' -> 22
  Just 'g' -> 20
  Just 'y' -> 20
  Just 'p' -> 19
  Just 'b' -> 15
  Just 'v' -> 10
  Just 'k' -> 8
  Just 'j' -> 2
  Just 'x' -> 2
  Just 'q' -> 1
  Just 'z' -> 1
  _ -> 5
