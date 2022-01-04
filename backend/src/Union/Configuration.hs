-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Union.Configuration
  ( defaultUnionConfig

  -- * Configuration types
  , DatabaseConfig (..)
  , UnionConfig (..)

  -- * Tools
  , loadConfig

  -- * Command-line options
  , UnionOptions (..)
  , unionOpts
  ) where

import Relude

import Data.Aeson (FromJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (NominalDiffTime)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Network.Wai.Handler.Warp (Port)
import Options.Applicative
  (Parser, execParser, fullDesc, help, helper, info, long, metavar, progDesc,
  short, strOption)

import Core.Json (jsonCamelOptions)
import Core.Logging (Severity(..))


-- | Database configuration.
data DatabaseConfig = DatabaseConfig
  { dcPoolSize    :: !Int
    -- ^ Pool-size.
  , dcTimeout     :: !NominalDiffTime
    -- ^ An amount of time for which an unused resource is kept open.
    -- The smallest acceptable value is 0.5 seconds.
  , dcCredentials :: !Text
    -- ^ Connection settings.
  } deriving stock (Generic, Eq, Show)
$(deriveJSON jsonCamelOptions 'DatabaseConfig)

-- | Union configuration.
data UnionConfig = UnionConfig
  { ucAppPort  :: !Port
  , ucDatabase :: !DatabaseConfig
  , ucSeverity :: !Severity
  } deriving stock (Generic, Eq, Show)
$(deriveJSON jsonCamelOptions 'UnionConfig)

-- | Helper to load config from yaml file.
loadConfig :: FromJSON settings => FilePath -> IO settings
loadConfig path = loadYamlSettings [path] [] useEnv

-- | Default Union config.
defaultUnionConfig :: UnionConfig
defaultUnionConfig = UnionConfig
  { ucAppPort = 8080
  , ucDatabase = DatabaseConfig
    { dcPoolSize = 100
    , dcTimeout = 5
    , dcCredentials = "host=localhost port=5432 user=union dbname=union"
    }
  , ucSeverity = Info
  }

-- | Options, passed via command line arguments.
newtype UnionOptions = UnionOptions
  { uoConfig  :: Maybe FilePath
  } deriving stock (Generic, Eq, Show)

unionOpts :: IO UnionOptions
unionOpts = execParser . info (unionOptsParser <**> helper) $
  fullDesc <> progDesc "Union web API"

unionOptsParser :: Parser UnionOptions
unionOptsParser = UnionOptions
  <$> configParser

configParser :: Parser (Maybe FilePath)
configParser = optional . strOption $
  short 'c' <>
  long "config" <>
  metavar "UNION_CONFIG" <>
  help "Path to configuration file."
