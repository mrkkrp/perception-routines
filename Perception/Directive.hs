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
  | Ground
  | Sky
  | Sounds
  deriving (Enum, Bounded, Eq, Show)

all :: [Directive]
all = [minBound .. maxBound]

name :: Directive -> Text
name = \case
  Breath -> "breath"
  Ground -> "ground/floor"
  Sky -> "sky"
  Sounds -> "sounds"

mnemonic :: Directive -> Char
mnemonic = \case
  Breath -> 'b'
  Ground -> 'g'
  Sky -> 's'
  Sounds -> 'o'

text :: Directive -> Text
text = \case
  Breath -> "Take a slow deep breath, pay attention to qualities of the air."
  Ground ->
    "What does the ground/floor feel like? How hard is it? Try to move around while\n\
    \paying attention to how the ground/floor reacts."
  Sky ->
    "Concentrate on the sky. How does its color vary? How big does it feel? Are\n\
    \there any clouds? If yes, how many different kinds? What is their\n\
    \character and speed? Stare long enough to see how clouds actually move."
  Sounds ->
    "Concentrate on what you hear. How many distinct sound sources are there?\n\
    \Pay attention to the spacial position and character of sounds. Separate\n\
    \them mentally, keep track of new sound sources that appear."

precondition :: Directive -> Natural -> State -> Bool
precondition directive _n st = case directive of
  Breath -> True
  Ground -> True
  Sky -> stEnvironment st == Outdoors
  Sounds -> True

effect :: Directive -> Natural -> State -> State
effect _directive _n st = st {stStamina = stStamina st - 1}
