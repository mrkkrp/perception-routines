{-# LANGUAGE LambdaCase #-}

module Perception.Routine.Mnemonic
  ( attachBaseWeights,
    applyPhoneticBias,
  )
where

import Numeric.Natural

attachBaseWeights ::
  (Functor f) =>
  (a -> Char) ->
  f a ->
  f (Natural, a)
attachBaseWeights mnemonic xs = g <$> xs
  where
    g x = (letterWeight (mnemonic x), x)

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

data Class = Vowel | Consonant
  deriving (Eq, Show)

classify :: Char -> Class
classify x = if x `elem` "aeiouy" then Vowel else Consonant

applyPhoneticBias ::
  (Functor f) =>
  (a -> Char) ->
  Maybe a ->
  f (Natural, a) ->
  f (Natural, a)
applyPhoneticBias _ Nothing xs = xs
applyPhoneticBias mnemonic (Just p) xs = g <$> xs
  where
    g (n, a) =
      if classify (mnemonic a) == classify (mnemonic p)
        then (n, a)
        else (n * 2, a)
