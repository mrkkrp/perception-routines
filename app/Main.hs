{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Text.IO qualified as Text
import Data.Version (showVersion)
import Development.GitRev
import Numeric.Natural
import Options.Applicative
import Paths_perception_routines (version)
import Perception.Routine qualified as Routine
import Perception.State
import System.Random.SplitMix

-- | Entry point of the program.
main :: IO ()
main = do
  Opts {..} <- execParser optsParserInfo
  let st =
        State
          { stEnvironment = optEnvironment,
            stStamina = optStamina
          }
  g <- case optSeed of
    Nothing -> initSMGen
    Just seed -> return (mkSMGen (fromIntegral seed))
  let routines = Routine.makeN optRoutinesToGenerate st g
  forM_ routines (Text.putStrLn . Routine.render)

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
    optRoutinesToGenerate :: Natural
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

----------------------------------------------------------------------------
-- Helpers

naturalReader :: ReadM Natural
naturalReader = auto

environmentReader :: ReadM Environment
environmentReader = eitherReader $ \case
  "outdoors" -> Right Outdoors
  "indoors" -> Right Indoors
  s -> Left $ "unknown environment: " ++ s
