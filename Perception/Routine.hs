-- | This module implements generation of perception routines.
module Perception.Routine
  ( Routine,
    size,
    sampleN,
    sample,
    mnemonic,
    tactics,
  )
where

import Control.Monad (replicateM)
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural
import Perception.Gen (Gen)
import Perception.Tactic (Tactic)
import Perception.Tactic qualified as Tactic
import Prelude hiding (id)
import Prelude qualified

-- | Perception routine.
newtype Routine = Routine [Tactic]

-- | Routine size.
size :: Natural
size = 5

-- | Generate multiple perception routines.
sampleN ::
  -- | How many perception routines to generate?
  Natural ->
  -- | The generated routines
  Gen [Routine]
sampleN n = replicateM (fromIntegral n) sample

-- | Generate a perception routine.
sample :: Gen Routine
sample = do
  xs <- go size [] Prelude.id
  pure (Routine xs)
  where
    go s tacticsSoFar acc =
      if s > 0
        then do
          m <- Tactic.sample tacticsSoFar
          case m of
            Nothing -> pure (acc [])
            Just r -> go (s - 1) (r : tacticsSoFar) (acc . (r :))
        else pure (acc [])

-- | Render a 'Routine' as a mnemonic phrase.
mnemonic :: Routine -> Text
mnemonic (Routine xs) =
  (capitalize . Text.pack . fmap Tactic.mnemonic) xs
  where
    capitalize txt =
      case Text.uncons txt of
        Nothing -> txt
        Just (a, as) -> Text.cons (Char.toUpper a) as

-- | Project 'Tactic's from a 'Routine'.
tactics :: Routine -> [Tactic]
tactics (Routine xs) = xs
