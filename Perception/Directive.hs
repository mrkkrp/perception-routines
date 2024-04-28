{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Perception.State.Internal (State (..))
import Prelude hiding (all)

data Directive
  = Breath
  deriving (Enum, Bounded, Eq)

all :: [Directive]
all = [minBound .. maxBound]

name :: Directive -> Text
name = \case
  Breath -> "Breath"

mnemonic :: Directive -> Char
mnemonic = \case
  Breath -> 'b'

text :: Directive -> Text
text = \case
  Breath -> "Take a slow deep breath, pay attention to qualities of the air."

precondition :: Directive -> Natural -> State -> Bool
precondition directive _n _st = case directive of
  Breath -> True

effect :: Directive -> Natural -> State -> State
effect _directive _n st = st {stStamina = stStamina st - 1}
