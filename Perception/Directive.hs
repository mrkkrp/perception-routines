{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines all perception directives.
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

-- | All known perception directives.
data Directive
  = Breath
  | ClosestArea
  | ColorDifferentiation
  | CompletionOfInvisibleAreas
  | ConstancyThroughAngle
  | ConstancyThroughMotion
  | ConstancyThroughTime
  | ExpectationsOfView
  | FarthestArea
  | GoOut
  | Ground
  | Pressure
  | Shadows
  | Sky
  | Smell
  | Sounds
  | TactileExpectations
  | VisualReconstruction
  | Walk
  deriving (Enum, Bounded, Eq, Show)

-- | An enumeration of all different values of 'Directive'.
all :: [Directive]
all = [minBound .. maxBound]

-- | The human-friendly name of a 'Directive'.
name :: Directive -> Text
name = \case
  Breath -> "breath"
  ClosestArea -> "closest area"
  ColorDifferentiation -> "color differentiation"
  CompletionOfInvisibleAreas -> "completion of invisible areas"
  ConstancyThroughAngle -> "constancy through angle of view"
  ConstancyThroughMotion -> "constancy through motion"
  ConstancyThroughTime -> "constancy through time"
  ExpectationsOfView -> "expectations of view"
  FarthestArea -> "farthest area"
  GoOut -> "go out"
  Ground -> "walk (with focus on the ground/floor)"
  Pressure -> "pressure"
  Shadows -> "shadows"
  Sky -> "sky"
  Smell -> "smell"
  Sounds -> "sounds"
  TactileExpectations -> "tactile expectations"
  VisualReconstruction -> "visual reconstruction"
  Walk -> "walk (with focus on spacial movement)"

-- | The mnemonic of a 'Directive'.
mnemonic :: Directive -> Char
mnemonic = \case
  Breath -> 'e'
  ClosestArea -> 'l'
  ColorDifferentiation -> 'c'
  CompletionOfInvisibleAreas -> 'r'
  ConstancyThroughAngle -> 'a'
  ConstancyThroughMotion -> 'm'
  ConstancyThroughTime -> 'i'
  ExpectationsOfView -> 'v'
  FarthestArea -> 'f'
  GoOut -> 'x'
  Ground -> 'g'
  Pressure -> 'p'
  Shadows -> 'h'
  Sky -> 'y'
  Smell -> 's'
  Sounds -> 'o'
  TactileExpectations -> 't'
  VisualReconstruction -> 'u'
  Walk -> 'w'

-- | The comprehensive description of a 'Directive'.
text :: Directive -> Text
text = \case
  Breath -> "Take a slow deep breath, pay attention to qualities of the air."
  ClosestArea ->
    "Scrutinize the area that immediately surrounds you, say, within your arm's\n\
    \length. Do not hesitate to use touch for exploration where it makes sense."
  CompletionOfInvisibleAreas ->
    "Choose a partially occluded object, or an object with an invisible area\n\
    \(e.g. back side) that you feel you can visualize in your head. Visualize\n\
    \the invisible area of the object, then either move yourself or move the\n\
    \object (if possible) so as to reveal that part to yourself. Compare your\n\
    \expectation with what you actually see."
  ColorDifferentiation ->
    "Find two closest colors belonging to different objects. How do they\n\
    \differ?"
  ConstancyThroughAngle ->
    "Choose an object or an area. Move around it in various ways so as to\n\
    \explore it from different angles. Concentrate on the fact that it is the\n\
    \same thing no matter the angle of view."
  ConstancyThroughMotion ->
    "Attend to everything that is in movement around you. Is everything still,\n\
    \or are most things moving? What about things that seem to be still but\n\
    \move on closer inspection? How does the character of movement change over\n\
    \time?"
  ConstancyThroughTime ->
    "Choose an object or an area. Scrutinize it carefully over a period of\n\
    \time. How does it change in time, if at all? Concentrate on how the thing\n\
    \you are attending to persists through time; try to see it as an active\n\
    \process rather than something gratuitous."
  ExpectationsOfView ->
    "Imagine what you would see from a certain position and angle of view\n\
    \different from where you are now. Go there and compare your expectation\n\
    \with reality."
  FarthestArea ->
    "Scrutinize the farthest area (according to your estimation, excluding the\n\
    \sky and celestial objects) that you can observe from your position. Try to\n\
    \evaluate how much space is accessible to your vision in every direction.\n\
    \Form a mental image of the totality of that space."
  GoOut ->
    "Go out, pay attention to the change in materiality of the environment."
  Ground ->
    "Walk so as to change your position. What does the ground/floor feel like?\n\
    \How hard is it? How does it react when you step on it? What sounds does it\n\
    \make, if any?"
  Pressure ->
    "Choose a suitable surface and press on it with your hand. Maintain the\n\
    \pressure for some time, thus experiencing materiality of the surface.\n\
    \Mentally extrapolate this feeling to other similar surfaces that surround\n\
    \you. Try to give yourself an account of their materiality, materiality of\n\
    \your surroundings."
  Shadows ->
    "Investigate shadows: their position, length, character. This directive\n\
    \applies equally in the case of diffuse light. There will always be shaded\n\
    \areas, e.g. in vegetation. Where is the darkest and lightest shadow you\n\
    \can find?"
  Sky ->
    "Concentrate on the sky. How does its color vary? How big does it feel? Are\n\
    \there any clouds? If yes, how many different kinds? What is their\n\
    \character and speed? Stare long enough to see how clouds actually move."
  Smell ->
    "Choose a suitable object and smell it."
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
  VisualReconstruction ->
    "Look carefully at a scene of your choice. Now close your eyes and try to\n\
    \reconstruct the scene in your head. Open your eyes and compare your\n\
    \reconstruction with what you can actually see."
  Walk ->
    "Walk so as to change your position. Pay special attention to things that\n\
    \you pass by. How does the constantly changing angle of view and your\n\
    \movement contribute to your sense of spacial immersion? Try to concentrate\n\
    \on your being embedded in a 3d scene that you are traversing."

-- | Return the function that determines when a given 'Directive' can be
-- used.
precondition ::
  -- | The directive in question
  Directive ->
  -- | The index of this directive
  Natural ->
  -- | The perception routine state
  State ->
  -- | Can this directive be used?
  Bool
precondition directive _n st = case directive of
  GoOut -> stEnvironment st == Indoors
  Sky -> stEnvironment st == Outdoors
  _ -> True

-- | Apply the side-effects of a 'Directive' to a 'State'.
effect ::
  -- | The directive in question
  Directive ->
  -- | The index of this directive
  Natural ->
  -- | The state before
  State ->
  -- | The state after
  State
effect directive _n st =
  st
    { stEnvironment =
        if directive == GoOut
          then Outdoors
          else stEnvironment st,
      stStamina = stStamina st - 1
    }
