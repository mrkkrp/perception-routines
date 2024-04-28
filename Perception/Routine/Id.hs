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

data Id = Id Domain (Word64, Word64)
  deriving (Eq)

make :: Domain -> SMGen -> Id
make domain g = Id domain (unseedSMGen g)

render :: Id -> Text
render (Id (Domain env s) (x, y)) = Text.pack (printf "%c%d-%8x%8x" e s x y)
  where
    e = case env of
      Outdoors -> 'e'
      Indoors -> 'i'
