-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module describes Union configuration.
module Union.App.Configuration
  ( -- * Configuration types
    Config(..)
  , DatabaseConfig(..)

    -- * Tools
  , loadConfig
  ) where

import Relude

import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default(..))
import Data.Time.Clock (NominalDiffTime)
import Data.Yaml.Config (ignoreEnv, loadYamlSettings)
import Deriving.Aeson (CustomJSON(..))
import Network.Wai.Handler.Warp (Port)

import Core.Db (DbCredentials(..))
import Core.Json (JsonCamelCase)
import Core.Logger (Severity(..))
import Core.Sender (Phone(..))
import Core.Time (Seconds(..))


-- | Database configuration.
data DatabaseConfig = DatabaseConfig
  { dcPoolSize    :: !Int
    -- ^ Pool-size.
  , dcTimeout     :: !NominalDiffTime
    -- ^ An amount of time for which an unused resource is kept open.
    -- The smallest acceptable value is 0.5 seconds.
  , dcCredentials :: !DbCredentials
    -- ^ Connection settings.
  , dcMigrations  :: !FilePath
    -- ^ Path to migrations folder.
  }
  deriving stock (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JsonCamelCase "dc") DatabaseConfig

-- | Union configuration.
data Config = Config
  { cAppPort     :: !Port
    -- ^ Application web port.
  , cDatabase    :: !DatabaseConfig
    -- ^ Database configuration.
  , cSeverity    :: !Severity
    -- ^ Logging severity.
  , cJwtExpire   :: !Seconds
    -- ^ How many seconds JWT will be valid.
  , cSenderPhone :: !(Maybe Phone)
    -- ^ Phone number for sender service.
  }
  deriving stock (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JsonCamelCase "c") Config

-- | Default Union config.
instance Default Config where
  def = Config
    { cAppPort     = 8080
    , cDatabase    = DatabaseConfig
      { dcPoolSize    = 100
      , dcTimeout     = 5
      , dcCredentials = DbCredentials
        "host=localhost port=5432 user=union dbname=union"
      , dcMigrations  = "./migrations"
      }
    , cSeverity    = Info
    , cJwtExpire   = Seconds 86400
    , cSenderPhone = Nothing
    }

-- | Helper to load config from yaml file.
loadConfig :: FromJSON settings => FilePath -> IO settings
loadConfig path = loadYamlSettings [path] [] ignoreEnv
