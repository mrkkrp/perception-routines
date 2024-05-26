-- | This module implements generation of perception routines.
module Perception.Routine
  ( Routine,
    makeN,
    make,
    id,
    mnemonic,
  )
where

import Data.Bifunctor (first)
import Data.Char qualified as Char
import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural
import Perception.Directive (Directive)
import Perception.Directive qualified as Directive
import Perception.Routine.Domain (Domain (..))
import Perception.Routine.Id qualified as Routine.Id
import Perception.Routine.Mnemonic
import Perception.Routine.Probability
import System.Random.SplitMix
import Prelude hiding (id)
import Prelude qualified

-- | Perception routine.
data Routine = Routine Routine.Id.Id [NonEmpty Directive]

-- | Generate multiple perception routines.
makeN ::
  -- | How many perception routines to generate?
  Natural ->
  -- | The domain to use
  Domain ->
  -- | The state of the preudo-random generator
  SMGen ->
  -- | The generated routines
  [Routine]
makeN n0 domain g0 = unfoldr makeOne (n0, g0)
  where
    makeOne (0, _) = Nothing
    makeOne (n, g) =
      let (routine, g') = make domain g
       in Just (routine, (n - 1, g'))

-- | Generate a perception routine.
make ::
  -- | The domain to use
  Domain ->
  -- | The state of the preudo-random generator
  SMGen ->
  -- | The generated routine and the updated preudo-random generator state
  (Routine, SMGen)
make domain@(Domain e s) g0 =
  first
    (Routine (Routine.Id.make domain g0))
    (go g0 0 Nothing Nothing Prelude.id)
  where
    go g n lastDirective lastMnemonic acc =
      if n >= s
        then (finalize acc, g)
        else
          let precondition x =
                Directive.compatible e x && Just x /= lastDirective
           in case NonEmpty.nonEmpty (filter precondition Directive.all) of
                Nothing -> (finalize acc, g)
                Just xs ->
                  let (mdirective, g') =
                        weightedSample
                          g
                          ( assignWeights
                              Directive.mnemonic
                              lastMnemonic
                              xs
                          )
                      lastDirective' =
                        case mdirective of
                          Nothing -> lastDirective
                          Just directive -> Just directive
                      lastMnemonic' = Directive.mnemonic <$> mdirective
                   in go
                        g'
                        (n + 1)
                        lastDirective'
                        lastMnemonic'
                        (acc . (mdirective :))
    finalize acc =
      mapMaybe
        f
        (NonEmpty.groupBy (\x y -> isJust x == isJust y) (acc []))
    f xs@(x :| _) =
      case x of
        Nothing -> Nothing
        Just _ -> Just (fmap fromJust xs) -- want it to fail loudly if my
        -- assumption is violated, hence partial 'fromJust'

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
