{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad (forM_, when)
import Data.FileEmbed
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import Data.Version (showVersion)
import Development.GitRev
import Numeric.Natural
import Options.Applicative
import Paths_perception_routines (version)
import Perception.Directive qualified as Directive
import Perception.Routine qualified as Routine
import Perception.Routine.Domain
import Perception.Routine.Id qualified as Routine.Id
import System.Random.SplitMix

-- | Entry point of the program.
main :: IO ()
main = do
  Opts {..} <- execParser optsParserInfo
  g <- case optSeed of
    Nothing -> initSMGen
    Just seed -> return (mkSMGen (fromIntegral seed))
  let domain = Domain optEnvironment optStamina
      routines = Routine.makeN optRoutinesToGenerate domain g
      m :: Int
      m =
        1 + floor (logBase 10.0 (fromIntegral optRoutinesToGenerate :: Double))
  forM_ (zip [1 ..] routines) $ \(i :: Int, routine) -> do
    when optPrintIndices $ do
      let i' = show i
          m' = m - length i'
      putStr (replicate m' ' ' ++ i' ++ ". ")
    Text.putStr (Routine.mnemonic routine)
    when optPrintIds $ do
      Text.putStr " ("
      Text.putStr (Routine.Id.render (Routine.id routine))
      Text.putStr ")"
    Text.putStrLn ""
  when optPrintExplanations $ do
    Text.putStrLn ""
    Text.putStrLn (Text.decodeUtf8 $(embedFile "intro.txt"))
    forM_ (sortOn Directive.mnemonic Directive.all) $ \d -> do
      putChar (Directive.mnemonic d)
      Text.putStr " = "
      Text.putStr (Directive.name d)
      Text.putStrLn ""
      Text.putStr (indentText (Directive.text d))
    Text.putStrLn ""

----------------------------------------------------------------------------
-- Command line options parsing

-- | Command line options.
data Opts = Opts
  { -- | Seed
    optSeed :: Maybe Natural,
    -- | Environment (outside or inside).
    optEnvironment :: Environment,
    -- | Stamina.
    optStamina :: Natural,
    -- | The number of routines to generate.
    optRoutinesToGenerate :: Natural,
    -- | Print indices of the generated perception routines.
    optPrintIndices :: Bool,
    -- | Print routine ids.
    optPrintIds :: Bool,
    -- | Print explanations of each directive.
    optPrintExplanations :: Bool
  }

optsParserInfo :: ParserInfo Opts
optsParserInfo =
  info (helper <*> ver <*> optsParser) . mconcat $
    [fullDesc]
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
    <$> (optional . option naturalReader . mconcat)
      [ long "seed",
        short 's',
        metavar "SEED",
        help "Seed of the preudo-random generator (if not set random seed is used)"
      ]
    <*> (option environmentReader . mconcat)
      [ long "env",
        short 'e',
        metavar "ENVIRONMENT",
        value Outdoors,
        help "Environment type: 'outdoors' or 'indoors'"
      ]
    <*> (option naturalReader . mconcat)
      [ long "stamina",
        short 't',
        metavar "STAMINA",
        value 10,
        help "Stamina available per routine"
      ]
    <*> (option naturalReader . mconcat)
      [ short 'n',
        metavar "N",
        value 1,
        help "Number of routines to generate"
      ]
    <*> (switch . mconcat)
      [ long "indices",
        short 'i',
        help "Print indices of the generated perception routines"
      ]
    <*> (switch . mconcat)
      [ long "ids",
        short 'I',
        help "Print routine ids"
      ]
    <*> (switch . mconcat)
      [ long "explain",
        short 'x',
        help "Print explanations for all directives."
      ]

----------------------------------------------------------------------------
-- Helpers

naturalReader :: ReadM Natural
naturalReader = auto

environmentReader :: ReadM Environment
environmentReader = eitherReader $ \case
  "outdoors" -> Right Outdoors
  "indoors" -> Right Indoors
  s -> Left $ "unknown environment: " ++ s

indentText :: Text -> Text
indentText = Text.unlines . fmap ("  " <>) . Text.lines
