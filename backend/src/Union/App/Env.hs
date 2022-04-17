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

    -- * Constraints
  , WithDb
  , WithLog
  , WithError
  , WithJwt
  ) where

import Relude

import Servant.Server.Generic (AsServerT)

import qualified Core

import Core.Db (DbPool)
import Core.Has (Has)
import Core.Jwt (JwtPayload, JwtSecret(JwtSecret), JwtToken, MonadJwt)
import Core.Logging (Log, LogAction, logError)
import Core.Time (Seconds)

import Union.App.Configuration
  (Config(..), DatabaseConfig(..), defaultConfig, loadConfig)
import Union.App.Error (Error)


-- | Main application monad.
type App = Core.App Error Env

-- | Union application server.
type Union = AsServerT App

-- | Constraint to actions with DB access.
type WithDb m = Core.WithDb Env m

-- | Constraint to actions with logging.
type WithLog m = Core.WithLog Env Log m

-- | Constraint to actions that can throw and catch pure errors with call-stack.
type WithError m = Core.WithError Error m

-- | Constraint to actions with Jwt generation / verification.
type WithJwt m = MonadJwt Int64 m


-- | Helper to simplify deriving for 'Env'.
type EnvField f = Core.Field f Env

-- | Union environment.
data Env = Env
  { eConfig    :: !Config
  , eDbPool    :: !DbPool
  , eLogger    :: !(LogAction App Log)
  , eJwtSecret :: !JwtSecret
  }
  deriving (Has Config)    via EnvField "eConfig"
  deriving (Has DbPool)    via EnvField "eDbPool"
  deriving (Has JwtSecret) via EnvField "eJwtSecret"

-- | Instance to specify how to get and update the 'LogAction' stored inside
-- the 'Env'.
instance Core.HasLog Env Log App where
  getLogAction :: Env -> LogAction App Log
  getLogAction = eLogger
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction App Log -> Env -> Env
  setLogAction newLogAction env = env { eLogger = newLogAction }
  {-# INLINE setLogAction #-}

-- | Instance to specify how to generate and verify a 'JwtToken'.
instance (Integral a, Bounded a) => MonadJwt a App where
  mkJwtToken :: Seconds -> JwtPayload a -> App JwtToken
  mkJwtToken expire payload = do
    secret <- Core.grab @JwtSecret
    Core.mkJwtTokenImpl Core.encodeIntIdPayload secret expire payload

  verifyJwtToken :: JwtToken -> App (Maybe (JwtPayload a))
  verifyJwtToken token = do
    secret <- Core.grab @JwtSecret
    Core.verifyJwtTokenImpl Core.decodeIntIdPayload secret token

-- | Create Union 'Env' with given path to 'Config'.
buildEnv :: Maybe FilePath -> IO Env
buildEnv path = do
  eConfig <- maybe (pure defaultConfig) loadConfig path
  let DatabaseConfig {..} = cDatabase eConfig
  let eLogger             = Core.setLogger $ cSeverity eConfig
  eJwtSecret <- JwtSecret <$> Core.mkRandomString 10
  Core.withDb dcPoolSize dcTimeout dcCredentials $ \eDbPool -> pure Env { .. }

-- | Runs provided action with new 'Env'.
runWithEnv :: Maybe FilePath -> App a -> IO a
runWithEnv path action = buildEnv path >>= flip Core.runApp action
{-# INLINE runWithEnv #-}

-- | Helper to kill the app.
kill :: (MonadReader env m, MonadFail m, WithLog m) => Text -> m b
kill msg = withFrozenCallStack (logError msg >> fail "Check logs 🠑")
{-# INLINE kill #-}
