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
import Data.Char
import Data.List (unfoldr)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural
import Perception.Directive (Directive)
import Perception.Directive qualified as Directive
import Perception.Routine.Domain (Domain)
import Perception.Routine.Id qualified as Routine.Id
import Perception.Routine.Mnemonic
import Perception.Routine.Probability
import Perception.State qualified as State
import System.Random.SplitMix
import Prelude hiding (id)
import Prelude qualified

-- | Perception routine.
data Routine = Routine Routine.Id.Id [Directive]

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
make domain g0 =
  first
    (Routine (Routine.Id.make domain g0))
    (go (State.init domain) g0 0 noHistory Prelude.id)
  where
    go st g n history acc =
      if State.stamina st == 0 || n >= maxDirectivesPerRoutine
        then (acc [], g)
        else
          let precondition x =
                Directive.precondition x n st && Just x /= lastDirective history
           in case NonEmpty.nonEmpty (filter precondition Directive.all) of
                Nothing -> (acc [], g)
                Just xs ->
                  let (directive, g') =
                        weightedSample
                          g
                          ( applyPhoneticBias
                              Directive.mnemonic
                              history
                              (attachBaseWeights Directive.mnemonic xs)
                          )
                      st' = Directive.effect directive n st
                   in go
                        st'
                        g'
                        (n + 1)
                        (addToHistory history directive)
                        (acc . (directive :))
    -- This is a simple set of helpers to maintain a history of 2 last
    -- directives.
    noHistory = Nothing
    lastDirective = fmap fst
    addToHistory Nothing x = Just (x, Nothing)
    addToHistory (Just (h, _)) x = Just (x, Just h)

-- | Project the routine id from a routine.
id :: Routine -> Routine.Id.Id
id (Routine id' _) = id'

-- | Render a 'Routine' as a mnemonic phrase.
mnemonic :: Routine -> Text
mnemonic (Routine _ xs) =
  (capitalize . Text.pack . breakIntoWords . fmap Directive.mnemonic) xs
  where
    capitalize txt =
      case Text.uncons txt of
        Nothing -> txt
        Just (a, as) -> Text.cons (toUpper a) as

-- | The maximal number of directives that can be included in a perception
-- routine. This is mostly for safely so as to prevent diverging generation
-- even when the directives fail to e.g. decrease the stamina.
maxDirectivesPerRoutine :: Natural
maxDirectivesPerRoutine = fromIntegral (length Directive.all)
