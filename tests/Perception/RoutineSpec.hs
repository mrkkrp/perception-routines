module Perception.RoutineSpec (spec) where

import Data.Char qualified as Char
import Data.List qualified
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
    it "does not generate routines with repeating directives" $
      property $ \domain seed gamma -> do
        Routine.mnemonic (routine domain seed gamma)
          `shouldSatisfy` noDirectiveRepetition
    it "generates routines of the requested size" $
      property $ \domain@(Domain size) seed gamma -> do
        Routine.mnemonic (routine domain seed gamma)
          `shouldSatisfy` nDirectivesEq size

routine :: Domain -> Word64 -> Word64 -> Routine
routine domain seed gamma =
  fst $
    Gen.run
      (seedSMGen seed gamma)
      (Routine.sample domain)

noDirectiveRepetition :: Text -> Bool
noDirectiveRepetition t = t0 == t1
  where
    t0 = Text.unpack t
    t1 = Data.List.nub t0

nDirectivesEq :: Natural -> Text -> Bool
nDirectivesEq n txt =
  Text.length (Text.filter (not . Char.isSpace) txt) == fromIntegral n
