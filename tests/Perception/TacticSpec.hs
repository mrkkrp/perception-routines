module Perception.TacticSpec (spec) where

import Control.Monad (forM_)
import Data.List ((\\))
import Perception.Tactic qualified as Tactic
import Test.Hspec

spec :: Spec
spec = do
  describe "mnemonic" $ do
    it "assigns mnemonics without clashes" $ do
      let pairWithMnemonic x = (Tactic.mnemonic x, x)
          allTacticsWithMnemonics = pairWithMnemonic <$> Tactic.all
          allMnemonics = fst <$> allTacticsWithMnemonics
          lengthIsOne xs = length xs == 1
      forM_ allMnemonics $ \mnemonic ->
        filter (\(m, _) -> m == mnemonic) allTacticsWithMnemonics
          `shouldSatisfy` lengthIsOne
    it "assigns a tactic for each letter" $ do
      let assignedLetters = Tactic.mnemonic <$> Tactic.all
          allLetters = ['a' .. 'z']
      allLetters \\ assignedLetters `shouldBe` ""
