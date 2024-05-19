module Perception.RoutineSpec (spec) where

import Data.Char qualified as Char
import Data.List (group)
import Data.Text (Text)
import Data.Text qualified as Text
import Perception.Routine qualified as Routine
import System.Random.SplitMix
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "make" $
    it "does not generate routines with consequitively repeating directives" $
      property $ \domain seed gamma -> do
        let g = seedSMGen seed gamma
            (r, _) = Routine.make domain g
        Routine.mnemonic r `shouldNotSatisfy` consequitiveDirectiveRepetition

consequitiveDirectiveRepetition :: Text -> Bool
consequitiveDirectiveRepetition =
  any (\x -> length x > 1)
    . group
    . fmap Char.toLower
    . filter (not . Char.isSpace)
    . Text.unpack
