module Perception.RoutineSpec (spec) where

import Data.Char qualified as Char
import Data.List (group)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Numeric.Natural
import Perception.Gen qualified as Gen
import Perception.Routine (Routine)
import Perception.Routine qualified as Routine
import Perception.Routine.Domain (Domain (..))
import System.Random.SplitMix
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "sample" $ do
    it "does not generate routines with consecutively repeating directives" $
      property $ \domain seed gamma -> do
        Routine.mnemonic (routine domain seed gamma)
          `shouldNotSatisfy` consequitiveDirectiveRepetition
    it "generates routines of the requested length" $
      property $ \domain@(Domain _ stamina) seed gamma -> do
        Routine.mnemonic (routine domain seed gamma)
          `shouldSatisfy` nDirectivesEq stamina

routine :: Domain -> Word64 -> Word64 -> Routine
routine domain seed gamma =
  fst $
    Gen.run
      (seedSMGen seed gamma)
      (Routine.sample domain)

consequitiveDirectiveRepetition :: Text -> Bool
consequitiveDirectiveRepetition =
  any (\x -> length x > 1)
    . group
    . fmap Char.toLower
    . filter (not . Char.isSpace)
    . Text.unpack

nDirectivesEq :: Natural -> Text -> Bool
nDirectivesEq n txt =
  Text.length (Text.filter (not . Char.isSpace) txt) == fromIntegral n
