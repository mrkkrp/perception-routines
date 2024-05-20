{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Aeson (Value (..), toJSON)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char qualified as Char
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy.IO qualified as TextLazy
import Data.Version (showVersion)
import Development.GitRev
import Numeric.Natural
import Options.Applicative
import Paths_perception_routines (version)
import Text.Mustache
import Text.Mustache.Compile.TH qualified as TH

-- | Entry point of the program.
main :: IO ()
main = do
  Opts {..} <- execParser optsParserInfo
  txt <- Text.readFile optExampleText
  let (r, _) = Text.foldl' updateLetterWeights (initLetterWeights, Nothing) txt
  TextLazy.writeFile
    "Perception/Routine/Mnemonic/LetterWeight.hs"
    ( renderMustache
        letterWeightTemplate
        (prepareForRendering (adjustWorkBreakWeights r))
    )

type LetterWeights = Map (Maybe Char, Maybe Char) Natural

-- | Initialize letter weights.
initLetterWeights :: LetterWeights
initLetterWeights =
  Map.fromList $
    [((Just x, Just y), defaultWeight) | x <- allLetters, y <- allLetters]
      ++ [((Nothing, Just x), defaultWeight) | x <- allLetters]
      ++ [((Just x, Nothing), defaultWeight) | x <- allLetters]
  where
    allLetters = ['a' .. 'z']
    defaultWeight = 0

-- | Update letter weights for a single 'Char'.
updateLetterWeights ::
  -- | Weights so far and the previous 'Char' if any
  (LetterWeights, Maybe Char) ->
  -- | 'Char' to consider
  Char ->
  -- | Updated weights and the new last 'Char'
  (LetterWeights, Maybe Char)
updateLetterWeights (m, prev) actual = (m', actual')
  where
    actualLower = Char.toLower actual
    actual' =
      if Char.isAsciiLower actualLower
        then Just actualLower
        else Nothing
    m' = case (prev, actual') of
      (Nothing, Nothing) -> m
      _ -> Map.adjust (+ 1) (prev, actual') m

-- | Adjust weights for word breaks.
adjustWorkBreakWeights :: LetterWeights -> LetterWeights
adjustWorkBreakWeights = Map.mapWithKey f
  where
    f (_prev, actual) weight =
      if isNothing actual
        then floor (fromIntegral weight * 0.7 :: Double)
        else weight

-- | Prepare values for being interpolated in the template.
prepareForRendering :: LetterWeights -> Value
prepareForRendering = toJSON . fmap f . Map.toList
  where
    f :: ((Maybe Char, Maybe Char), Natural) -> KeyMap String
    f ((prev, actual), weight) =
      KeyMap.fromList
        [("prev", show prev), ("actual", show actual), ("weight", show weight)]

-- | A pre-compiled mustache template.
letterWeightTemplate :: Template
letterWeightTemplate = $(TH.compileMustacheFile "template/LetterWeight.mustache")

----------------------------------------------------------------------------
-- Command line options parsing

newtype Opts = Opts
  { optExampleText :: FilePath
  }

optsParserInfo :: ParserInfo Opts
optsParserInfo =
  info (helper <*> ver <*> optsParser) fullDesc
  where
    ver :: Parser (a -> a)
    ver =
      infoOption verStr . mconcat $
        [ long "version",
          short 'v',
          help "Print version of the program"
        ]
    verStr =
      unwords
        [ "gen-letter-weights",
          showVersion version,
          $gitBranch,
          $gitHash
        ]

optsParser :: Parser Opts
optsParser =
  Opts
    <$> (argument str . mconcat)
      [ metavar "FILE",
        help "Plain text file to use to obtain letter weights for the Markov chain model"
      ]
