{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines all perception directives.
module Perception.Directive
  ( Directive (..),
    all,
    name,
    mnemonicKeyword,
    frequencyRank,
    normativeDirectiveProbabilities,
    affinity,
    mnemonicAffinity,
    probabilityAffinity,
    mnemonic,
    text,
    sample,
  )
where

import Data.Algorithm.Assignment (assign)
import Data.Bifunctor (second)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural
import Perception.Gen (Gen)
import Perception.Gen qualified as Gen
import Perception.Routine.Mnemonic (WithWordBreaks (..), assignWeights)
import Perception.Routine.Mnemonic.LetterFrequency (letterFrequencies)
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
  | Gravity
  | Ground
  | Light
  | OutsidePerspective
  | Pressure
  | SeparationThroughFocus
  | Shadows
  | Sky
  | Smell
  | Sounds
  | TactileExpectations
  | UnusualView
  | VisualReconstruction
  | Walk
  deriving (Enum, Bounded, Eq, Ord, Show)

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
  Gravity -> "gravity and supporting forces"
  Ground -> "walk (with focus on the ground/floor)"
  Light -> "light"
  OutsidePerspective -> "outside perspective"
  Pressure -> "pressure"
  SeparationThroughFocus -> "separation through focus"
  Shadows -> "shadows"
  Sky -> "sky"
  Smell -> "smell"
  Sounds -> "sounds"
  TactileExpectations -> "tactile expectations"
  UnusualView -> "view from an unusual position/angle "
  VisualReconstruction -> "visual reconstruction"
  Walk -> "walk (with focus on spatial movement)"

-- | Keywords that are used for assigning letter mnemonics. These should be
-- in lower case and should not contain spaces or punctuation.
mnemonicKeyword :: Directive -> Text
mnemonicKeyword = \case
  Breath -> "breath"
  ClosestArea -> "closest"
  CompletionOfInvisibleAreas -> "invisible"
  ConstancyOfColor -> "color"
  ConstancyThroughAngle -> "angle"
  ConstancyThroughDistance -> "distance"
  ConstancyThroughMotion -> "motion"
  ConstancyThroughTime -> "time"
  ExpectationsOfSound -> "sound"
  ExpectationsOfView -> "view"
  FarthestArea -> "farthest"
  Gravity -> "gravity"
  Ground -> "ground"
  Light -> "light"
  OutsidePerspective -> "outside"
  Pressure -> "pressure"
  SeparationThroughFocus -> "focus"
  Shadows -> "shadows"
  Sky -> "sky"
  Smell -> "smell"
  Sounds -> "sounds"
  TactileExpectations -> "tactile"
  UnusualView -> "unusual"
  VisualReconstruction -> "reconstruction"
  Walk -> "walk"

-- | The frequency rank of a 'Directive'.
frequencyRank :: Directive -> Natural
frequencyRank = \case
  -- Breath is always nice. It is simple and does not take much time.
  Breath -> 4
  -- These are my favorites. The corresponding phenomenological realizations
  -- seem to be evident and the directives are fun to follow.
  ConstancyOfColor -> 3
  ConstancyThroughAngle -> 3
  ConstancyThroughDistance -> 3
  ConstancyThroughMotion -> 3
  ConstancyThroughTime -> 3
  ExpectationsOfSound -> 3
  Ground -> 3
  Pressure -> 3
  SeparationThroughFocus -> 3
  Smell -> 3
  Sounds -> 3
  TactileExpectations -> 3
  UnusualView -> 3
  Walk -> 3
  -- These are also good, but perhaps take a little longer than those in the
  -- previous group or their principle seems to be a bit more contrived, so
  -- it makes sense for them to be a little bit more rare.
  CompletionOfInvisibleAreas -> 2
  ExpectationsOfView -> 2
  Gravity -> 2
  Light -> 2
  Shadows -> 2
  Sky -> 2
  -- Not my favorites, added for the sake of variety.
  ClosestArea -> 1
  FarthestArea -> 1
  OutsidePerspective -> 1
  VisualReconstruction -> 1

-- | Directives paired with their normative probabilities.
normativeDirectiveProbabilities :: [(Directive, Double)]
normativeDirectiveProbabilities =
  second adjustedRankToProbability <$> allWithAdjustedRanks
  where
    allWithAdjustedRanks = zip all (adjustRank . frequencyRank <$> all)
    adjustRank :: Natural -> Double
    adjustRank x = (2.0 :: Double) ^ x
    totalAdjustedRankPoints = sum (snd <$> allWithAdjustedRanks)
    adjustedRankToProbability x = x / totalAdjustedRankPoints

-- | The map from directives to their 'Char' mnemonics. This map is the
-- result of a compromise between mnemonic affinity of directives and
-- letters and probability affinity between letter probabilities in English
-- and the normative probabilities of the directives as per their
-- 'frequencyRank'.
mnemonicMap :: Map Directive Char
mnemonicMap =
  Map.fromList $
    f <$> assign cost normativeDirectiveProbabilities letterFrequencies
  where
    f ((d, _), (ch, _)) = (d, ch)
    cost d ch = floor ((1.0 - affinity d ch) * 10000.0)
{-# NOINLINE mnemonicMap #-}

-- | Calculate affinity between a directive and a letter. The result is
-- between 0 (no match) and 1 (perfect match), inclusive.
affinity ::
  -- | A directive paired with its normative probability
  (Directive, Double) ->
  -- | A letter paired with its probability in English
  (Char, Double) ->
  Double
affinity (d, p') (ch, p) =
  mnemonicAffinity (mnemonicKeyword d) ch
    * probabilityAffinity p' p

-- | Calculate affinity between a mnemonic keyword and a given letter. The
-- result is between 0 (no match) and 1 (perfect match), inclusive.
mnemonicAffinity ::
  -- | The mnemonic keyword
  Text ->
  -- | A letter
  Char ->
  Double
mnemonicAffinity k ch =
  max 0.05 $
    1.0 - maybe 1.0 ((* 0.1) . fromIntegral) (Text.findIndex (== ch) k)

-- | Calculate affinity between two probabilities. The result is between 0
-- (no match) and 1 (perfect match), inclusive.
probabilityAffinity :: Double -> Double -> Double
probabilityAffinity x y =
  let n = max x y
   in (n - abs (x - y)) / n

-- | The mnemonic of a 'Directive'.
mnemonic :: Directive -> Char
mnemonic k = mnemonicMap Map.! k

-- | The comprehensive description of a 'Directive'.
text :: Directive -> Text
text = \case
  Breath ->
    "Take a slow deep breath, pay attention to qualities of the air."
  ClosestArea ->
    "Scrutinize the area that immediately surrounds you, say, within your arm's\n\
    \length. What qualities does it have in comparison to objects that are\n\
    \farther away?"
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
    "Scrutinize the farthest area (according to your estimation, excluding the\n\
    \sky and celestial objects) that you can observe from your position. Try to\n\
    \evaluate how much space is accessible to your vision in every direction.\n\
    \Form a mental image of the totality of that space."
  Gravity ->
    "Attempt to imagine the weight of various objects that you observe and the\n\
    \pressure they exercise on their support (floor, ground, etc.). What forces\n\
    \allow these objects to remain in their current position? ‘Visualize’ in\n\
    \your mind such things as rigidity, elasticity, inner tensions. If\n\
    \feasible, attempt to lift and move around objects of interest to\n\
    \experience the gravitational forces they are subjected to and to verify\n\
    \your intuitions."
  Ground ->
    "Walk so as to change your position. What does the ground/floor feel like?\n\
    \How hard is it? How does it react when you step on it? What sounds does it\n\
    \make, if any?"
  Light ->
    "Appreciate the light that illuminates the scene that you are observing. Do\n\
    \not take it for granted. One way to do it is to imagine that all\n\
    \observable light is only a short flash that started recently and can cease\n\
    \at any moment."
  OutsidePerspective ->
    "Visualize your body in its current position and environment from an\n\
    \outside perspective. Attempt to “disconnect” from your familiar sense of\n\
    \self and perceive yourself as an independent observer would."
  Pressure ->
    "Choose a suitable surface and press on it with your hand. Maintain the\n\
    \pressure for some time, thus experiencing materiality of the surface. Note\n\
    \its texture and temperature. Mentally extrapolate this feeling to other\n\
    \similar surfaces that surround you. Try to give yourself an account of\n\
    \their materiality."
  SeparationThroughFocus ->
    "Concentrate on an object of your choice. Note how it seems to stand out in\n\
    \relation to its surroundings. What qualities does it have in comparison to\n\
    \other objects that you are not focused on?"
  Shadows ->
    "Investigate shadows: their position, length, character. This directive\n\
    \applies equally in the case of diffuse light. There will always be shaded\n\
    \areas, e.g. in the vegetation. Explicitly relate shadows to the angle and\n\
    \character of light and the occluding object. Where is the darkest and\n\
    \lightest shadow you can find?"
  Sky ->
    "Concentrate on the sky. How does its color vary? How big does it feel? Are\n\
    \there any clouds? If yes, how many different kinds? What is their\n\
    \character and speed? Stare long enough to see how clouds actually move."
  Smell ->
    "Choose a suitable object and smell it."
  Sounds ->
    "Concentrate on what you hear. Note that unlike other sense modalities we\n\
    \phenomenologically experience sounds from the past as part of the present.\n\
    \How many distinct sound sources are there? Pay attention to the spatial\n\
    \position and the character of sounds. How do reflection and reverberation\n\
    \contribute to the way sounds describe space? Keep track of new sound\n\
    \sources that appear. Closing your eyes might be helpful."
  TactileExpectations ->
    "Choose an object or a surface. Imagine what it would feel like to grasp,\n\
    \touch, or perform some other manipulation with that object or surface.\n\
    \Perform the imagined action while maintaining visual contact with the\n\
    \chosen object/surface and compare your expectation with reality."
  UnusualView ->
    "Observe the environment you are in from an unusual position and/or angle\n\
    \of view. Note the fact that it is the same scene and yet it looks\n\
    \different because more of its essence is revealed."
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
-- preceding directives in the routine.
sample ::
  -- | Up to 2 preceding directives
  Maybe (Maybe Directive, WithWordBreaks Directive) ->
  -- | The resulting directive
  Gen (WithWordBreaks Directive)
sample precedingDirectives = go
  where
    go = do
      mdirective <-
        Gen.weighted $
          assignWeights
            mnemonic
            precedingChars
            eligibleDirectives
      case (mdirective, precedingDirective) of
        (Nothing, Just x) -> pure (WordBreak x)
        -- Two consecutive word breaks should not happen, but try again.
        (Nothing, Nothing) -> go
        (Just x, _) -> pure (WordConstituent x)
    precedingChars = case precedingDirectives of
      Nothing -> Nothing
      Just (md, WordConstituent x) -> Just (mnemonic <$> md, mnemonic x)
      Just (_, WordBreak _) -> Nothing
    precedingDirective = case precedingDirectives of
      Nothing -> Nothing
      Just (_, WordConstituent x) -> Just x
      Just (_, WordBreak x) -> Just x
    eligibleDirectives = NonEmpty.fromList (filter eligibleDirective all)
    eligibleDirective x = Just x /= precedingDirective
