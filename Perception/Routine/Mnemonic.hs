module Perception.Routine.Mnemonic
  ( applyPhoneticBias,
  )
where

import Numeric.Natural

applyPhoneticBias ::
  (Functor f) =>
  (a -> Char) ->
  Maybe a ->
  f (Natural, a) ->
  f (Natural, a)
applyPhoneticBias _ Nothing xs = xs
applyPhoneticBias _ (Just _prevChar) xs = xs
