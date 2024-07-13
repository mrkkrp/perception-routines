{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad (forM_, when)
import Data.FileEmbed
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Version (showVersion)
import Development.GitRev
import Numeric.Natural
import Options.Applicative
import Paths_perception_routines (version)
import Perception.Directive (Directive)
import Perception.Directive qualified as Directive
import Perception.Gen qualified as Gen
import Perception.Routine qualified as Routine
import Perception.Routine.Mnemonic.LetterFrequency (letterFrequencies)
import System.Random.SplitMix
import Text.Printf (printf)

-- | Entry point of the program.
main :: IO ()
main = do
  Opts {..} <- execParser optsParserInfo
  g <- case optSeed of
    Nothing -> initSMGen
    Just seed -> return (mkSMGen (fromIntegral seed))
  let (routines, _) = Gen.run g (Routine.sampleN optRoutinesToGenerate)
      m :: Int
      m =
        1 + floor (logBase 10.0 (fromIntegral optRoutinesToGenerate :: Double))
  forM_ (zip [1 ..] routines) $ \(i :: Int, routine) -> do
    when optPrintIndices $ do
      let i' = show i
          m' = m - length i'
      putStr (replicate m' ' ' ++ i' ++ ". ")
    Text.putStr (Routine.mnemonic routine)
    Text.putStrLn ""
  when optPrintExplanations $ do
    Text.putStrLn ""
    Text.putStrLn (Text.decodeUtf8 $(embedFile "intro.txt"))
    forM_ (sortOn Directive.mnemonic Directive.all) $ \directive -> do
      putChar (Directive.mnemonic directive)
      Text.putStr " = "
      Text.putStr (Directive.name directive)
      Text.putStrLn ""
      when optExplainMnemonics $ do
        Text.putStr (indentText 8 (mnemonicJustification directive))
      Text.putStr (indentText 2 (Directive.text directive))
    Text.putStrLn ""

----------------------------------------------------------------------------
-- Command line options parsing

-- | Command line options.
data Opts = Opts
  { -- | Seed
    optSeed :: Maybe Natural,
    -- | The number of routines to generate.
    optRoutinesToGenerate :: Natural,
    -- | Print indices of the generated perception routines.
    optPrintIndices :: Bool,
    -- | Print explanations of each directive.
    optPrintExplanations :: Bool,
    -- | Also print explanations for assignment of mnemonics.
    optExplainMnemonics :: Bool
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
        [ "proutine",
          showVersion version,
          $gitBranch,
          $gitHash
        ]

optsParser :: Parser Opts
optsParser =
  Opts
    <$> (optional . option auto . mconcat)
      [ long "seed",
        short 's',
        metavar "SEED",
        help "Seed of the preudo-random generator (if not set a random seed is used)"
      ]
    <*> (option auto . mconcat)
      [ short 'n',
        metavar "N",
        value 1,
        showDefault,
        help "Number of routines to generate"
      ]
    <*> (switch . mconcat)
      [ long "indices",
        short 'i',
        help "Print indices of the generated perception routines"
      ]
    <*> (switch . mconcat)
      [ long "explain",
        short 'x',
        help "Print explanations for all directives."
      ]
    <*> (switch . mconcat)
      [ long "explain-mnemonics",
        short 'm',
        help "Also print explanations for assignment of mnemonics"
      ]

----------------------------------------------------------------------------
-- Helpers

mnemonicJustification :: Directive -> Text
mnemonicJustification directive =
  Text.unlines
    [ "Affinity (overall): " <> percent affinity,
      "Mnemonic affinity: "
        <> percent mnemonicAffinity
        <> " ('"
        <> Text.singleton mnemonic
        <> "'"
        <> " for \""
        <> mnemonicKeyword
        <> "\")",
      "Probability affinity: "
        <> percent probabilityAffinity
        <> " ("
        <> percent normativeProbability
        <> " normative vs "
        <> percent mnemonicProbability
        <> " letter)"
    ]
  where
    percent x = Text.pack (printf "%.2f%%" (x * 100.0))
    affinity =
      Directive.affinity
        (directive, normativeProbability)
        (mnemonic, mnemonicProbability)
    mnemonicAffinity =
      Directive.mnemonicAffinity mnemonicKeyword mnemonic
    probabilityAffinity =
      Directive.probabilityAffinity normativeProbability mnemonicProbability
    mnemonic = Directive.mnemonic directive
    mnemonicKeyword = Directive.mnemonicKeyword directive
    mnemonicProbability =
      fromJust $
        lookup mnemonic letterFrequencies
    normativeProbability =
      fromJust $
        lookup directive Directive.normativeDirectiveProbabilities

indentText :: Int -> Text -> Text
indentText n = Text.unlines . fmap (Text.replicate n " " <>) . Text.lines
