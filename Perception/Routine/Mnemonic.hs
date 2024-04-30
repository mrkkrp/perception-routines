module Perception.Routine.Mnemonic
  ( applyPhoneticBias,
  )
where

import Numeric.Natural

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
