-- | This module implements generation of perception routines.
module Perception.Routine
  ( Routine,
    sampleN,
    sample,
    id,
    mnemonic,
    directives,
  )
where

import Control.Monad (replicateM)
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural
import Perception.Directive (Directive)
import Perception.Directive qualified as Directive
import Perception.Gen (Gen)
import Perception.Gen qualified as Gen
import Perception.Routine.Domain (Domain (..))
import Perception.Routine.Id qualified as Routine.Id
import Prelude hiding (id)
import Prelude qualified

-- | Perception routine.
data Routine = Routine Routine.Id.Id [Directive]

-- | Generate multiple perception routines.
sampleN ::
  -- | How many perception routines to generate?
  Natural ->
  -- | The domain to use
  Domain ->
  -- | The generated routines
  Gen [Routine]
sampleN n domain = replicateM (fromIntegral n) (sample domain)

-- | Generate a perception routine.
sample ::
  -- | The domain to use
  Domain ->
  -- | The generated routine and the updated preudo-random generator state
  Gen Routine
sample domain@(Domain s0) = do
  g <- Gen.askSMGen
  xs <- go s0 [] Prelude.id
  pure (Routine (Routine.Id.make domain g) xs)
  where
    go s directivesSoFar acc =
      if s > 0
        then do
          m <- Directive.sample directivesSoFar
          case m of
            Nothing -> pure (acc [])
            Just r -> go (s - 1) (r : directivesSoFar) (acc . (r :))
        else pure (acc [])

-- | Project the routine id from a routine.
id :: Routine -> Routine.Id.Id
id (Routine id' _) = id'

-- | Render a 'Routine' as a mnemonic phrase.
mnemonic :: Routine -> Text
mnemonic (Routine _ xs) =
  (capitalize . Text.pack . fmap Directive.mnemonic) xs
  where
    capitalize txt =
      case Text.uncons txt of
        Nothing -> txt
        Just (a, as) -> Text.cons (Char.toUpper a) as

-- | Project directives from a 'Routine'.
directives :: Routine -> [Directive]
directives (Routine _ xs) = xs
