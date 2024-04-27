module Perception.Directive
  ( Directive (..),
    all,
    name,
    mnemonic,
    text,
    precondition,
    effect,
  )
where

import Data.Text (Text)
import Numeric.Natural
import Perception.State (State)
import Prelude hiding (all)

data Directive = Foo
  deriving (Enum, Bounded, Eq)

all :: [Directive]
all = [minBound .. maxBound]

name :: Directive -> Text
name = undefined

mnemonic :: Directive -> Char
mnemonic = undefined

text :: Directive -> Text
text = undefined

precondition :: Directive -> Natural -> State -> Bool
precondition = undefined

effect :: Directive -> Natural -> State -> State
effect = undefined
