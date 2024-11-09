{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines all perception tactics.
module Perception.Tactic
  ( Tactic (..),
    all,
    name,
    mnemonicKeyword,
    frequencyRank,
    normativeProbabilities,
    affinity,
    mnemonicAffinity,
    probabilityAffinity,
    mnemonic,
    directive,
    sample,
  )
where

import Data.Algorithm.Assignment (assign)
import Data.Bifunctor (second)
import Data.List ((\\))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural
import Perception.Gen (Gen)
import Perception.Gen qualified as Gen
import Perception.Routine.Mnemonic (assignWeights)
import Perception.Routine.Mnemonic.LetterFrequency (letterFrequencies)
import Prelude hiding (all)

-- | All known perception tactics.
data Tactic
  = Breath
  | CompletionOfInvisibleAreas
  | ConstancyOfColor
  | ConstancyThroughAngle
  | ConstancyThroughDistance
  | ConstancyThroughTime
  | ExpectationsOfExtent
  | ExpectationsOfSound
  | ExpectationsOfView
  | Ground
  | Light
  | OutsidePerspective
  | Pressure
  | SeparationThroughFocus
  | Shadows
  | Smell
  | Sounds
  | SpatialReference
  | TactileExpectations
  | UnusualView
  | VisualReconstruction
  | Walk
  deriving (Enum, Bounded, Eq, Ord, Show)

-- | An enumeration of all different values of 'Tactic'.
all :: [Tactic]
all = [minBound .. maxBound]

-- | The human-friendly name of a 'Tactic'.
name :: Tactic -> Text
name = \case
  Breath -> "breath"
  CompletionOfInvisibleAreas -> "completion of invisible areas"
  ConstancyOfColor -> "constancy of color"
  ConstancyThroughAngle -> "constancy through angle of view"
  ConstancyThroughDistance -> "constancy through distance"
  ConstancyThroughTime -> "constancy through time"
  ExpectationsOfExtent -> "expectations of extent"
  ExpectationsOfSound -> "expectations of sound"
  ExpectationsOfView -> "expectations of view"
  Ground -> "walk (with focus on the ground/floor)"
  Light -> "light"
  OutsidePerspective -> "outside perspective"
  Pressure -> "pressure"
  SeparationThroughFocus -> "separation through focus"
  Shadows -> "shadows"
  Smell -> "smell"
  Sounds -> "sounds"
  SpatialReference -> "spatial reference"
  TactileExpectations -> "tactile expectations"
  UnusualView -> "view from an unusual position/angle "
  VisualReconstruction -> "visual reconstruction"
  Walk -> "walk (with focus on spatial movement)"

-- | Keywords that are used for assigning letter mnemonics. These should be
-- in lower case and should not contain spaces or punctuation.
mnemonicKeyword :: Tactic -> Text
mnemonicKeyword = \case
  Breath -> "breath"
  CompletionOfInvisibleAreas -> "invisible"
  ConstancyOfColor -> "color"
  ConstancyThroughAngle -> "angle"
  ConstancyThroughDistance -> "distance"
  ConstancyThroughTime -> "time"
  ExpectationsOfExtent -> "extent"
  ExpectationsOfSound -> "sound"
  ExpectationsOfView -> "view"
  Ground -> "ground"
  Light -> "light"
  OutsidePerspective -> "outside"
  Pressure -> "pressure"
  SeparationThroughFocus -> "focus"
  Shadows -> "shadows"
  Smell -> "smell"
  Sounds -> "sounds"
  SpatialReference -> "reference"
  TactileExpectations -> "tactile"
  UnusualView -> "unusual"
  VisualReconstruction -> "reconstruction"
  Walk -> "walk"

-- | The frequency rank of a 'Tactic'.
frequencyRank :: Tactic -> Natural
frequencyRank = \case
  -- Breath is always nice. It is simple and does not take much time.
  Breath -> 4
  -- These are my favorites. The corresponding phenomenological realizations
  -- seem to be evident and the tactics are fun to follow.
  ConstancyOfColor -> 3
  ConstancyThroughAngle -> 3
  ConstancyThroughDistance -> 3
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
  ExpectationsOfExtent -> 2
  ExpectationsOfView -> 2
  Light -> 2
  Shadows -> 2
  SpatialReference -> 2
  -- Not my favorites, added for the sake of variety.
  OutsidePerspective -> 1
  VisualReconstruction -> 1

-- | Tactics paired with their normative probabilities.
normativeProbabilities :: [(Tactic, Double)]
normativeProbabilities =
  second adjustedRankToProbability <$> allWithAdjustedRanks
  where
    allWithAdjustedRanks = zip all (adjustRank . frequencyRank <$> all)
    adjustRank :: Natural -> Double
    adjustRank x = (2.0 :: Double) ^ x
    totalAdjustedRankPoints = sum (snd <$> allWithAdjustedRanks)
    adjustedRankToProbability x = x / totalAdjustedRankPoints

-- | The map from tactics to their 'Char' mnemonics. This map is the result
-- of a compromise between a mnemonic affinity of tactics and letters and a
-- probability affinity between letter probabilities in English and the
-- normative probabilities of the tactics as per their 'frequencyRank'.
mnemonicMap :: Map Tactic Char
mnemonicMap =
  Map.fromList $
    f <$> assign cost normativeProbabilities letterFrequencies
  where
    f ((d, _), (ch, _)) = (d, ch)
    cost d ch = floor ((1.0 - affinity d ch) * 10000.0)
{-# NOINLINE mnemonicMap #-}

-- | Calculate the affinity between a tactic and a letter. The result is
-- between 0 (no match) and 1 (perfect match), inclusive.
affinity ::
  -- | A tactic paired with its normative probability
  (Tactic, Double) ->
  -- | A letter paired with its probability in English
  (Char, Double) ->
  Double
affinity (d, p') (ch, p) =
  mnemonicAffinity (mnemonicKeyword d) ch
    * probabilityAffinity p' p

-- | Calculate the affinity between a mnemonic keyword and a given letter.
-- The result is between 0 (no match) and 1 (perfect match), inclusive.
mnemonicAffinity ::
  -- | The mnemonic keyword
  Text ->
  -- | A letter
  Char ->
  Double
mnemonicAffinity k ch =
  max 0.05 $
    1.0 - maybe 1.0 ((* 0.1) . fromIntegral) (Text.findIndex (== ch) k)

-- | Calculate the affinity between two probabilities. The result is between
-- 0 (no match) and 1 (perfect match), inclusive.
probabilityAffinity :: Double -> Double -> Double
probabilityAffinity x y =
  let n = max x y
   in (n - abs (x - y)) / n

-- | The mnemonic of a 'Tactic'.
mnemonic :: Tactic -> Char
mnemonic k = mnemonicMap Map.! k

-- | The comprehensive description of a 'Tactic'.
directive :: Tactic -> Text
directive = \case
  Breath ->
    "Take a slow deep breath, pay attention to qualities of the air."
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
  ConstancyThroughTime ->
    "Choose an object or an area. Scrutinize it carefully over a period of\n\
    \time. How does it change in time, if at all? Concentrate on how the thing\n\
    \you are attending to persists through time; try to see it as an active\n\
    \process rather than something gratuitous."
  ExpectationsOfExtent ->
    "Choose an object or an area that is at least 3 meters away from you.\n\
    \Attempt to estimate its size, e.g. in relation to parts of your body.\n\
    \Visualize yourself next to the chosen object/area, then approach it and\n\
    \verify your intuitions."
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
  Ground ->
    "Walk so as to change your position. What does the ground/floor feel like?\n\
    \How hard is it? How does it react when you step on it? What sounds does it\n\
    \make, if any?"
  Light ->
    "Appreciate the light that illuminates the scene that you are observing. Do\n\
    \not take it for granted. One way to do it is to imagine that all\n\
    \observable light is only a short flash that has just started and can cease\n\
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
    "Investigate shadows: their position, length, character. This tactic\n\
    \applies equally in the case of diffuse light. There will always be shaded\n\
    \areas, e.g. in the vegetation. Explicitly relate shadows to the angle and\n\
    \character of light and the occluding object."
  Smell ->
    "Choose a suitable object and smell it."
  Sounds ->
    "Concentrate on what you hear. Note that unlike other sense modalities we\n\
    \phenomenologically experience sounds from the past as part of the present.\n\
    \How many distinct sound sources are there? Pay attention to their spatial\n\
    \position and character. How do reflection and reverberation contribute to\n\
    \the way sounds describe space? Keep track of new sound sources that\n\
    \appear. Closing your eyes might be helpful."
  SpatialReference ->
    "Think of a reference object that you cannot directly observe, then form an\n\
    \idea of how some other object or area of your choice is oriented in\n\
    \relation to the reference object. How does this affect your sense of space\n\
    \perception?"
  TactileExpectations ->
    "Choose an object or a surface. Imagine what it would feel like to grasp,\n\
    \touch, or perform some other manipulation with that object or surface.\n\
    \Perform the imagined action while maintaining visual contact with the\n\
    \chosen object/surface and compare your expectation with reality."
  UnusualView ->
    "Observe the environment you are in from an unusual position and/or angle\n\
    \of view. Note the fact that it is the same scene and yet it looks\n\
    \different because more of its nature is revealed."
  VisualReconstruction ->
    "Look carefully at a scene of your choice. Now close your eyes and try to\n\
    \reconstruct the scene in your head. Open your eyes and compare your\n\
    \reconstruction with what you can actually see."
  Walk ->
    "Walk so as to change your position. Pay special attention to things that\n\
    \you pass by. How does the constantly changing angle of view and your\n\
    \movement contribute to your sense of spatial immersion? Try to concentrate\n\
    \on your being embedded in the 3d scene that you are traversing."

-- | Produce a random 'Tactic' sample given the preceding 'Tactic's in the
-- routine.
sample ::
  -- | 'Tactic's used so far (latest first)
  [Tactic] ->
  -- | The resulting 'Tactic'
  Gen (Maybe Tactic)
sample precedingTactics = go
  where
    go = case NonEmpty.nonEmpty (all \\ precedingTactics) of
      Nothing -> return Nothing
      Just eligibleTactics ->
        Just
          <$> Gen.weighted
            ( assignWeights
                mnemonic
                precedingChars
                eligibleTactics
            )
    precedingChars = case precedingTactics of
      [] -> Nothing
      [x] -> Just (Nothing, mnemonic x)
      (x0 : x1 : _) -> Just (Just (mnemonic x1), mnemonic x0)
