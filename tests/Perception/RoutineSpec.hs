module Perception.RoutineSpec (spec) where

import Data.Char qualified as Char
import Data.List qualified
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Numeric.Natural
import Perception.Gen qualified as Gen
import Perception.Routine (Routine)
import Perception.Routine qualified as Routine
import Perception.Tactic qualified as Tactic
import System.Random.SplitMix
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "sample" $ do
    it "does not generate routines with repeating tactics" $
      property $ \seed gamma -> do
        Routine.mnemonic (routine seed gamma)
          `shouldSatisfy` noTacticRepetition
    it "generates routines of the requested size" $
      property $ \seed gamma -> do
        Routine.mnemonic (routine seed gamma)
          `shouldSatisfy` nTacticsEq Routine.size
    it "all tactics have a chance to appear" $ do
      let xs = routineN 1_000 0 0
      Set.fromList (concatMap Routine.tactics xs)
        `shouldBe` Set.fromList Tactic.all

routine :: Word64 -> Word64 -> Routine
routine seed gamma =
  fst $
    Gen.run
      (seedSMGen seed gamma)
      Routine.sample

routineN :: Natural -> Word64 -> Word64 -> [Routine]
routineN n seed gamma =
  fst $
    Gen.run
      (seedSMGen seed gamma)
      (Routine.sampleN n)

noTacticRepetition :: Text -> Bool
noTacticRepetition t = t0 == t1
  where
    t0 = Text.unpack t
    t1 = Data.List.nub t0

nTacticsEq :: Natural -> Text -> Bool
nTacticsEq n txt =
  Text.length (Text.filter (not . Char.isSpace) txt) == fromIntegral n
