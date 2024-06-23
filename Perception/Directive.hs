{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines all perception directives.
module Perception.Directive
  ( Directive (..),
    all,
    name,
    mnemonic,
    text,
    sample,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Perception.Gen (Gen)
import Perception.Gen qualified as Gen
import Perception.Routine.Mnemonic (WithWordBreaks (..), assignWeights)
import Prelude hiding (all)

-- | All known perception directives.
data Directive
  = Breath
  | ClosestArea
  | CompletionOfInvisibleAreas
  | ConstancyOfColor
  | ConstancyThroughAngle
  | ConstancyThroughDistance
  | ConstancyThroughMotion
  | ConstancyThroughTime
  | ExpectationsOfSound
  | ExpectationsOfView
  | FarthestArea
  | Ground
  | OutsidePerspective
  | Pressure
  | SeparationThroughFocus
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
  CompletionOfInvisibleAreas -> "completion of invisible areas"
  ConstancyOfColor -> "constancy of color"
  ConstancyThroughAngle -> "constancy through angle of view"
  ConstancyThroughDistance -> "constancy through distance"
  ConstancyThroughMotion -> "constancy through motion"
  ConstancyThroughTime -> "constancy through time"
  ExpectationsOfSound -> "expectations of sound"
  ExpectationsOfView -> "expectations of view"
  FarthestArea -> "farthest area"
  Ground -> "walk (with focus on the ground/floor)"
  OutsidePerspective -> "an outside perspective"
  Pressure -> "pressure"
  SeparationThroughFocus -> "separation through focus"
  Shadows -> "shadows"
  Sky -> "sky"
  Smell -> "smell"
  Sounds -> "sounds"
  TactileExpectations -> "tactile expectations"
  VisualReconstruction -> "visual reconstruction"
  Walk -> "walk (with focus on spatial movement)"

-- | The mnemonic of a 'Directive'.
mnemonic :: Directive -> Char
mnemonic = \case
  Breath -> 'e'
  ClosestArea -> 'l'
  CompletionOfInvisibleAreas -> 'r'
  ConstancyOfColor -> 'c'
  ConstancyThroughAngle -> 'a'
  ConstancyThroughDistance -> 'd'
  ConstancyThroughMotion -> 'm'
  ConstancyThroughTime -> 'i'
  ExpectationsOfSound -> 'n'
  ExpectationsOfView -> 'v'
  FarthestArea -> 'q'
  Ground -> 'g'
  OutsidePerspective -> 'x'
  Pressure -> 'p'
  SeparationThroughFocus -> 'f'
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
    -- FIXME is this good enough?
    "Scrutinize the area that immediately surrounds you, say, within your arm's\n\
    \length. Does it feel more accessible than the space farther away? Do not\n\
    \hesitate to use touch for exploration where it makes sense."
  CompletionOfInvisibleAreas ->
    "Choose a partially occluded object, or an object with an invisible area\n\
    \(e.g. back side) that you feel you can visualize in your head. Visualize\n\
    \the invisible area of the object, then change your position or move the\n\
    \object (if possible) so as to gradually reveal the invisible area. Compare\n\
    \your expectation with what you actually see."
  ConstancyOfColor ->
    "Choose an object or an area of uniform color or texture. Notice the fact\n\
    \that even though the color/texture is uniform there is likely a variation\n\
    \of tone due to uneven light (i.e. there are highlights and shadows, sharp\n\
    \or diffused). Recognize the variation in tone as different manifestations\n\
    \of the same color/texture that allow us to perceive it in a deeper way.\n\
    \What qualities of the incident light does this exercise reveal?"
  ConstancyThroughAngle ->
    "Concentrate on an object or an area of your choice. Gradually move around\n\
    \it in various ways while maintaining your attention on the chosen\n\
    \object/area so as to explore it from different angles. Concentrate on the\n\
    \fact that it is the same thing in the same position no matter the angle of\n\
    \view. What qualities of space does this exercise exemplify?"
  ConstancyThroughDistance ->
    "Concentrate on an object or an area of your choice. Gradually approach it\n\
    \or move farther away from it so as to change the distance between you and\n\
    \the chosen object/area. Concentrate on the fact that it is the same thing\n\
    \of the same physical size no matter the distance from which it is\n\
    \observed. What qualities of space does this exercise exemplify?"
  ConstancyThroughMotion ->
    "Attend to objects in motion around you. Is everything still, or are most\n\
    \things moving? What about things that seem to be still but move on closer\n\
    \inspection? Concentrate on the fact that the moving things retain their\n\
    \integrity and identity despite the movement. What can be said about the\n\
    \moving force?"
  ConstancyThroughTime ->
    "Choose an object or an area. Scrutinize it carefully over a period of\n\
    \time. How does it change in time, if at all? Concentrate on how the thing\n\
    \you are attending to persists through time; try to see it as an active\n\
    \process rather than something gratuitous."
  ExpectationsOfSound ->
    "Choose a suitable object and imagine what it would be like to provoke a\n\
    \sound by interacting with that object in a certain way. Carry out the\n\
    \interaction and compare your expectation with what you actually hear. How\n\
    \does the sound contribute to the sense of materiality of the chosen\n\
    \object?"
  ExpectationsOfView ->
    "Imagine what you would see from a certain position and an angle of view\n\
    \that is different from where you are now. Assume that position and the\n\
    \angle of view and compare your expectation with what you actually see."
  FarthestArea ->
    -- FIXME is this good enough?
    "Scrutinize the farthest area (according to your estimation, excluding the\n\
    \sky and celestial objects) that you can observe from your position. Try to\n\
    \evaluate how much space is accessible to your vision in every direction.\n\
    \Form a mental image of the totality of that space."
  Ground ->
    "Walk so as to change your position. What does the ground/floor feel like?\n\
    \How hard is it? How does it react when you step on it? What sounds does it\n\
    \make, if any?"
  OutsidePerspective ->
    "Visualize your body in its current position and environment from an\n\
    \outside perspective. Attempt to “disconnect” from your familiar sense of\n\
    \self and perceive yourself as an independent observer would."
  Pressure ->
    "Choose a suitable surface and press on it with your hand. Maintain the\n\
    \pressure for some time, thus experiencing materiality of the surface.\n\
    \Mentally extrapolate this feeling to other similar surfaces that surround\n\
    \you. Try to give yourself an account of their materiality."
  SeparationThroughFocus ->
    "Concentrate on an object of your choice. Note how it seems to stand out in\n\
    \relation to its surroundings. What qualities does it have in comparison to\n\
    \other objects that you are not focused on?"
  Shadows ->
    -- TODO realization missing?
    "Investigate shadows: their position, length, character. This directive\n\
    \applies equally in the case of diffuse light. There will always be shaded\n\
    \areas, e.g. in the vegetation. Where is the darkest and lightest shadow\n\
    \you can find?"
  Sky ->
    "Concentrate on the sky. How does its color vary? How big does it feel? Are\n\
    \there any clouds? If yes, how many different kinds? What is their\n\
    \character and speed? Stare long enough to see how clouds actually move."
  Smell ->
    "Choose a suitable object and smell it."
  Sounds ->
    "Concentrate on what you hear. How many distinct sound sources are there?\n\
    \Pay attention to the spatial position and the character of sounds.\n\
    \Separate them mentally, keep track of new sound sources that appear.\n\
    \Closing your eyes might be helpful."
  TactileExpectations ->
    "Choose an object or a surface. Imagine what it would feel like to grasp,\n\
    \touch, or perform some other manipulation with that object or surface.\n\
    \Perform the imagined action while maintaining visual contact with the\n\
    \chosen object/surface and compare your expectation with reality."
  VisualReconstruction ->
    "Look carefully at a scene of your choice. Now close your eyes and try to\n\
    \reconstruct the scene in your head. Open your eyes and compare your\n\
    \reconstruction with what you can actually see."
  Walk ->
    "Walk so as to change your position. Pay special attention to things that\n\
    \you pass by. How does the constantly changing angle of view and your\n\
    \movement contribute to your sense of spatial immersion? Try to concentrate\n\
    \on your being embedded in the 3d scene that you are traversing."

-- | Produce a random directive sample given the target environment and the
-- preceding directive in the routine.
sample ::
  -- | The previous directive
  Maybe (WithWordBreaks Directive) ->
  -- | The resulting directive
  Gen (WithWordBreaks Directive)
sample precedingDirective = go
  where
    go = do
      mdirective <-
        Gen.weighted $
          assignWeights
            mnemonic
            precedingChar
            eligibleDirectives
      case (mdirective, precedingDirective') of
        (Nothing, Just x) -> pure (WordBreak x)
        -- Two consecutive word breaks should not happen, but try again.
        (Nothing, Nothing) -> go
        (Just x, _) -> pure (WordConstituent x)
    precedingChar = case precedingDirective of
      Nothing -> Nothing
      Just (WordConstituent x) -> Just (mnemonic x)
      Just (WordBreak _) -> Nothing
    precedingDirective' = case precedingDirective of
      Nothing -> Nothing
      Just (WordConstituent x) -> Just x
      Just (WordBreak x) -> Just x
    eligibleDirectives = NonEmpty.fromList (filter eligibleDirective all)
    eligibleDirective x = Just x /= precedingDirective'
