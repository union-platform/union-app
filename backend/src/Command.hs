-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents command line interface for Union.
module Command
  ( Arguments(..)
  , Command(..)
  , getArguments
  ) where

import Relude

import Options.Applicative
  ( Parser
  , action
  , command
  , customExecParser
  , disambiguate
  , fullDesc
  , help
  , helper
  , hsubparser
  , info
  , long
  , prefs
  , progDesc
  , short
  , showHelpOnEmpty
  , showHelpOnError
  , strOption
  )

import Command.Migrations (MigrationsCommand, migrationsParser)


-- | Arguments, passed via command line arguments.
data Arguments = Arguments
  { aConfig  :: Maybe FilePath
  , aCommand :: Command
  }
  deriving stock (Show, Eq)

-- | Available Union commands.
data Command = Run | Migrations MigrationsCommand
  deriving stock (Show, Eq)

-- | Get arguments from command line.
getArguments :: IO Arguments
getArguments =
  customExecParser p . info (argumentsParser <**> helper) $ fullDesc <> progDesc
    "Union web-server"
  where p = prefs $ mconcat [disambiguate, showHelpOnEmpty, showHelpOnError]

-- | Parser for all available 'Arguments'.
argumentsParser :: Parser Arguments
argumentsParser = Arguments <$> configParser <*> commandParser

-- | Parser for config path.
configParser :: Parser (Maybe FilePath)
configParser = optional . strOption $ mconcat
  [short 'c', long "config", help "Path to configuration file.", action "file"]

-- | Parser for available 'Command's.
commandParser :: Parser Command
commandParser = hsubparser $ mconcat
  [ command "run" . info (pure Run) $ progDesc "Run Union server"
  , command "migrations" . info (Migrations <$> migrationsParser) $ progDesc
    "Union migrations tool"
  ]
