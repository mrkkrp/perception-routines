module Perception.DirectiveSpec (spec) where

import Control.Monad (forM_)
import Perception.Directive qualified as Directive
import Test.Hspec

spec :: Spec
spec = do
  describe "mnemonic" $ do
    it "assigns mnemonics without clashes" $ do
      let pairWithMnemonic x = (Directive.mnemonic x, x)
          allDirectivesWithMnemonics = pairWithMnemonic <$> Directive.all
          allMnemonics = fst <$> allDirectivesWithMnemonics
          lengthIsOne xs = length xs == 1
      forM_ allMnemonics $ \mnemonic ->
        filter (\(m, _) -> m == mnemonic) allDirectivesWithMnemonics
          `shouldSatisfy` lengthIsOne
