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
import Perception.Routine.Domain (Environment (..))
import Perception.State.Internal (State (..))
import Prelude hiding (all)

data Directive
  = Breath
  | Sky
  deriving (Enum, Bounded, Eq, Show)

all :: [Directive]
all = [minBound .. maxBound]

name :: Directive -> Text
name = \case
  Breath -> "breath"
  Sky -> "sky"

mnemonic :: Directive -> Char
mnemonic = \case
  Breath -> 'b'
  Sky -> 's'

text :: Directive -> Text
text = \case
  Breath -> "Take a slow deep breath, pay attention to qualities of the air."
  Sky ->
    "Concentrate on the sky, study it in minute detail. Are there any clouds?\n\
    \If yes, what is their character and speed?"

precondition :: Directive -> Natural -> State -> Bool
precondition directive _n st = case directive of
  Breath -> True
  Sky -> stEnvironment st == Outdoors

effect :: Directive -> Natural -> State -> State
effect _directive _n st = st {stStamina = stStamina st - 1}
