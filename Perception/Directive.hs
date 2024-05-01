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
  | ConstancyThroughAngle
  | ConstancyThroughTime
  | ExpectationsOfView
  | Ground
  | Pressure
  | Sky
  | Sounds
  | TactileExpectations
  deriving (Enum, Bounded, Eq, Show)

all :: [Directive]
all = [minBound .. maxBound]

name :: Directive -> Text
name = \case
  Breath -> "breath"
  ConstancyThroughAngle -> "constancy through angle of view"
  ConstancyThroughTime -> "constancy through time"
  ExpectationsOfView -> "expectations of view"
  Ground -> "ground/floor"
  Pressure -> "pressure"
  Sky -> "sky"
  Sounds -> "sounds"
  TactileExpectations -> "tactile expectations"

mnemonic :: Directive -> Char
mnemonic = \case
  Breath -> 'b'
  ConstancyThroughAngle -> 'a'
  ConstancyThroughTime -> 't'
  ExpectationsOfView -> 'v'
  Ground -> 'g'
  Pressure -> 'p'
  Sky -> 's'
  Sounds -> 'o'
  TactileExpectations -> 'e'

text :: Directive -> Text
text = \case
  Breath -> "Take a slow deep breath, pay attention to qualities of the air."
  ConstancyThroughAngle ->
    "Choose an object or an area. Move around it in various ways so as to\n\
    \explore it from different angles. Concentrate on the fact that it is the\n\
    \same thing no matter the angle of view."
  ConstancyThroughTime ->
    "Choose an object or an area. Scrutinize it carefully over a period of\n\
    \time. How does it change in time, if at all? Concentrate on how the thing\n\
    \you are attending to persists through time; try to see it as an active\n\
    \process rather than something gratuitous."
  ExpectationsOfView ->
    "Imagine what you would see from a certain position and angle of view\n\
    \different from where you are now. Go there and compare your expectation\n\
    \with reality."
  Ground ->
    "What does the ground/floor feel like? How hard is it? Try to move around while\n\
    \paying attention to how the ground/floor reacts, the sound it makes, if any."
  Pressure ->
    "Choose a suitable surface and press on it with your hand. Maintain the\n\
    \pressure for some time, thus experiencing materiality of the surface.\n\
    \Mentally extrapolate this feeling to other similar surfaces that surround\n\
    \you. Try to give yourself an account of their materiality, materiality of\n\
    \your surroundings."
  Sky ->
    "Concentrate on the sky. How does its color vary? How big does it feel? Are\n\
    \there any clouds? If yes, how many different kinds? What is their\n\
    \character and speed? Stare long enough to see how clouds actually move."
  Sounds ->
    "Concentrate on what you hear. How many distinct sound sources are there?\n\
    \Pay attention to the spacial position and character of sounds. Separate\n\
    \them mentally, keep track of new sound sources that appear. Closing your\n\
    \eyes might be helpful."
  TactileExpectations ->
    "Choose an object or a surface. Imagine what it would feel like to grasp,\n\
    \touch, or perform some other manipulation with that object or surface.\n\
    \Now, perform the imagined action and compare your expectation with\n\
    \reality."

precondition :: Directive -> Natural -> State -> Bool
precondition directive _n st = case directive of
  Sky -> stEnvironment st == Outdoors
  _ -> True

effect :: Directive -> Natural -> State -> State
effect _directive _n st = st {stStamina = stStamina st - 1}
