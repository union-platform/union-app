-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module describes Union environment.
module Union.App.Env
  ( App
  , Union

    -- * Environment
  , Env(..)
  , buildEnv
  , runWithEnv
  , kill

    -- * Constrains
  , WithDb
  , WithLog
  , WithError
  ) where

import Relude

import Servant.Server.Generic (AsServerT)

import qualified Core

import Core.Db (DbPool)
import Core.Has (Has)
import Core.Logging (Log, LogAction, logError)

import Union.App.Configuration (Config(..), DatabaseConfig (..), defaultConfig, loadConfig)
import Union.App.Error (Error)


-- | Main application monad.
type App = Core.App Error Env

-- | Union application server.
type Union = AsServerT App

-- | Constrain to actions with DB access.
type WithDb m = Core.WithDb Env m

-- | Constrain to actions with logging.
type WithLog m = Core.WithLog Env Log m

-- | Constrain to actions that can throw and catch pure errors with stack position.
type WithError m = Core.WithError Error m

-- | Union environment.
data Env = Env
  { eConfig :: !Config
  , eDbPool :: !DbPool
  , eLogger :: !(LogAction App Log)
  }
  deriving (Has Config) via Core.Field "eConfig" Env
  deriving (Has DbPool) via Core.Field "eDbPool" Env

-- | Instance to specify how to get and update the 'LogAction' stored inside
-- the 'Env'.
instance Core.HasLog Env Log App where
  getLogAction :: Env -> LogAction App Log
  getLogAction = eLogger
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction App Log -> Env -> Env
  setLogAction newLogAction env = env { eLogger = newLogAction }
  {-# INLINE setLogAction #-}

-- | Create Union 'Env' with given path to 'Config'.
buildEnv :: Maybe FilePath -> IO Env
buildEnv path = do
  eConfig <- maybe (pure defaultConfig) loadConfig path
  let DatabaseConfig {..} = cDatabase eConfig
  let eLogger             = Core.setLogger $ cSeverity eConfig
  Core.withDb dcPoolSize dcTimeout dcCredentials $ \eDbPool -> pure Env { .. }

-- | Runs provided action with new 'Env'.
runWithEnv :: Maybe FilePath -> App a -> IO a
runWithEnv path action = buildEnv path >>= flip Core.runApp action
{-# INLINE runWithEnv #-}

-- | Helper to kill the app.
kill :: (MonadReader env m, MonadFail m, WithLog m) => Text -> m b
kill msg = withFrozenCallStack (logError msg >> fail "Check logs 🠑")
{-# INLINE kill #-}
