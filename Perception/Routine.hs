-- | This module implements generation of perception routines.
module Perception.Routine
  ( Routine,
    sampleN,
    sample,
    id,
    mnemonic,
  )
where

import Control.Monad (replicateM)
import Data.Char qualified as Char
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural
import Perception.Directive (Directive)
import Perception.Directive qualified as Directive
import Perception.Gen (Gen)
import Perception.Gen qualified as Gen
import Perception.Routine.Domain (Domain (..))
import Perception.Routine.Id qualified as Routine.Id
import Perception.Routine.Mnemonic
import Perception.Routine.Mnemonic qualified as Mnemonic
import Prelude hiding (id)
import Prelude qualified

-- | Perception routine.
data Routine = Routine Routine.Id.Id [NonEmpty Directive]

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
  xs <- go s0 Nothing Prelude.id
  pure $ Routine (Routine.Id.make domain g) xs
  where
    go s prev acc =
      if s > 0
        then do
          r <- Directive.sample prev
          case r of
            WordConstituent _ -> go (s - 1) (Just r) (acc . (r :))
            WordBreak _ -> go s (Just r) (acc . (r :))
        else pure (finalize acc)
    finalize acc =
      mapMaybe
        f
        ( NonEmpty.groupBy
            ( \x y ->
                Mnemonic.isWordConstituent x == Mnemonic.isWordConstituent y
            )
            (acc [])
        )
    f xs@(x :| _) =
      case x of
        WordBreak _ -> Nothing
        WordConstituent _ ->
          -- I want it to fail loudly if my assumption is violated, hence
          -- the partial 'fromWordConstituent'.
          Just (fmap Mnemonic.fromWordConstituent xs)

-- | Project the routine id from a routine.
id :: Routine -> Routine.Id.Id
id (Routine id' _) = id'

-- | Render a 'Routine' as a mnemonic phrase.
mnemonic :: Routine -> Text
mnemonic (Routine _ xs) =
  ( capitalize
      . Text.unwords
      . fmap (Text.pack . NonEmpty.toList . fmap Directive.mnemonic)
  )
    xs
  where
    capitalize txt =
      case Text.uncons txt of
        Nothing -> txt
        Just (a, as) -> Text.cons (Char.toUpper a) as
