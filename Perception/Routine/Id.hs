-- | This module defined the id of a perception routine.
module Perception.Routine.Id
  ( Id,
    make,
    render,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word
import Perception.Routine.Domain
import System.Random.SplitMix
import Text.Printf

-- | The id of a perception routine is the combination of its 'Domain' and
-- the preudo-random generator state used to generate that perception
-- routine. In other words, it is the complete set of parameters that
-- control the result of generation. There is one-to-one mapping between
-- perception routine ids and the corresponding perception routines.
-- However, strictly speaking, certain ids will map to identical perception
-- routines.
data Id = Id Domain (Word64, Word64)
  deriving (Eq)

-- | Make a value of the 'Id' type.
make :: Domain -> SMGen -> Id
make domain g = Id domain (unseedSMGen g)

-- | Render an 'Id' as 'Text' in a human-friendly way.
render :: Id -> Text
render (Id (Domain s) (x, y)) = Text.pack (printf "%d-%016x%016x" s x y)
