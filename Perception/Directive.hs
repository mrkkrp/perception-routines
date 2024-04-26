module Perception.Directive
  ( Directive (..),
    name,
    mnemonic,
    text,
    precondition,
    effect,
  )
where

import Data.Text (Text)
import Perception.State (State)

data Directive = Foo

name :: Directive -> Text
name = undefined

mnemonic :: Directive -> Char
mnemonic = undefined

text :: Directive -> Text
text = undefined

precondition :: Directive -> State -> Bool
precondition = undefined

effect :: Directive -> State -> State
effect = undefined
