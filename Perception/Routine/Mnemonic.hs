module Perception.Routine.Mnemonic
  ( applyPhoneticBias,
  )
where

import Data.Ratio
import Numeric.Natural

applyPhoneticBias :: (a -> Char) -> Maybe a -> [(Ratio Natural, a)] -> [(Ratio Natural, a)]
applyPhoneticBias _ Nothing xs = xs
applyPhoneticBias _ (Just _prevChar) _xs = undefined
