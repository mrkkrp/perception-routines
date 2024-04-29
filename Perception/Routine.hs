module Perception.Routine
  ( Routine,
    makeN,
    make,
    id,
    mnemonic,
  )
where

import Data.Bifunctor (first)
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

data Routine = Routine Routine.Id.Id [Directive]

makeN :: Natural -> Domain -> SMGen -> [Routine]
makeN n0 domain g0 = unfoldr makeOne (n0, g0)
  where
    makeOne (0, _) = Nothing
    makeOne (n, g) =
      let (routine, g') = make domain g
       in Just (routine, (n - 1, g'))

make :: Domain -> SMGen -> (Routine, SMGen)
make domain g0 =
  first
    (Routine (Routine.Id.make domain g0))
    (go (State.init domain) g0 0 Nothing Prelude.id)
  where
    go st g n lastDirective acc =
      if State.stamina st == 0 || n >= maxDirectivesPerRoutine
        then (acc [], g)
        else
          let precondition x =
                Directive.precondition x n st && Just x /= lastDirective
           in case NonEmpty.nonEmpty (filter precondition Directive.all) of
                Nothing -> (acc [], g)
                Just xs ->
                  let (directive, g') =
                        weightedSample
                          g
                          ( applyPhoneticBias
                              Directive.mnemonic
                              lastDirective
                              (attachEqualWeights xs)
                          )
                      st' = Directive.effect directive n st
                   in go st' g' (n + 1) (Just directive) (acc . (directive :))

id :: Routine -> Routine.Id.Id
id (Routine id' _) = id'

mnemonic :: Routine -> Text
mnemonic (Routine _ xs) =
  (Text.toTitle . Text.pack . fmap Directive.mnemonic) xs

maxDirectivesPerRoutine :: Natural
maxDirectivesPerRoutine = fromIntegral (length Directive.all)
