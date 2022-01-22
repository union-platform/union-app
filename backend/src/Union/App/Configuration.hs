-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module describes Union configuration.
module Union.App.Configuration
  ( defaultConfig

    -- * Configuration types
  , DatabaseConfig(..)
  , Config(..)

    -- * Tools
  , loadConfig
  ) where

import Relude

import Data.Aeson (FromJSON)
import Data.Aeson.TH (deriveJSON)
import Data.Time.Clock (NominalDiffTime)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Network.Wai.Handler.Warp (Port)

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
  , dcMigrations  :: !FilePath
    -- ^ Path to migrations folder.
  }
  deriving stock (Generic, Eq, Show)
$(deriveJSON jsonCamelOptions 'DatabaseConfig)

-- | Union configuration.
data Config = Config
  { cAppPort  :: !Port
    -- ^ Application web port.
  , cDatabase :: !DatabaseConfig
    -- ^ Database configuration.
  , cSeverity :: !Severity
    -- ^ Logging severity.
  }
  deriving stock (Generic, Eq, Show)
$(deriveJSON jsonCamelOptions 'Config)

-- | Helper to load config from yaml file.
loadConfig :: FromJSON settings => FilePath -> IO settings
loadConfig path = loadYamlSettings [path] [] useEnv

-- | Default Union config.
defaultConfig :: Config
defaultConfig = Config
  { cAppPort  = 8080
  , cDatabase = DatabaseConfig
    { dcPoolSize    = 100
    , dcTimeout     = 5
    , dcCredentials = "host=localhost port=5432 user=union dbname=union"
    , dcMigrations  = "./migrations"
    }
  , cSeverity = Info
  }
