-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Union.Application
  ( Union
  , UnionServer
  , WithLog
  , WithDb
  , WithError
  , Env
  , ErrorType(..)
  , runUnion
  , union
  ) where

import Relude

import Hasql.Migration (MigrationError)
import Hasql.Pool (UsageError)
import Network.Wai.Handler.Warp (run)
import Servant (serve)

import qualified Core

import Core.Db (DbPool, migrate)
import Core.Error (catchError, toAppError, showErr)
import Core.Has (Has, grab)
import Core.Json (packJson)
import Core.Logging (Log, LogAction, logDebug, logError, logInfo)
import Union.API (api)
import Union.Configuration
  ( DatabaseConfig(..)
  , UnionConfig(..)
  , defaultUnionConfig
  , loadConfig
  )


-- | Main application monad.
type Union = Core.App ErrorType Env

-- | Type alias to specify application server.
type UnionServer = Core.AppServer Union

-- | Constrain to actions with logging.
type WithLog m = Core.WithLog Env Log m

-- | Constrain to actions with DB access.
type WithDb m = Core.WithDb Env m

-- | Constrain to actions that can throw and catch pure errors with stack position.
type WithError m = Core.WithError ErrorType m

-- | Union environment.
data Env = Env
  { eConfig :: !UnionConfig
  , eDbPool :: !DbPool
  , eLogger :: !(LogAction Union Log)
  }
  deriving (Has UnionConfig) via Core.Field "eConfig" Env
  deriving (Has DbPool)      via Core.Field "eDbPool" Env

-- | Instance to specify how to get and update the 'LogAction' stored inside
-- the 'Env'.
instance Core.HasLog Env Log Union where
  getLogAction :: Env -> LogAction Union Log
  getLogAction = eLogger
  {-# INLINE getLogAction #-}

  setLogAction :: LogAction Union Log -> Env -> Env
  setLogAction newLogAction env = env { eLogger = newLogAction }
  {-# INLINE setLogAction #-}

-- | Errors in Union.
data ErrorType
  = NotFound
  -- ^ General not found.
  | ServerError Text
  -- ^ Some exceptional circumstance has happened to stop execution and return.
  -- Optional text to provide some context in server logs.
  | NotAllowed Text
  -- ^ A required permission level was not met. Optional text to provide some context.
  | Invalid Text
  -- ^ Given inputs do not conform to the expected format or shape. Optional
  -- text to provide some context in server logs.
  | MissingHeader Text
  -- ^ Some header expected, but not present in header list.
  | HeaderDecodeError Text
  -- ^ An authentication header that was required was provided but not in a
  -- format that the server can understand.
  | DbError UsageError
  -- ^ Database specific errors.
  deriving stock (Show, Eq)

-- | Runs the web server which serves Union API.
union :: WithLog Union => Union ()
union = do
  config <- grab @UnionConfig
  logDebug $ "Configuration: \n" <> packJson config
  logInfo "Running migrations..."
  whenJustM (initDb config) $ \err ->
    logError ("Cannot apply migration: " <> showErr err) >> fail "Check logs 🠑"
  logInfo "Starting application..."
  liftIO $ run (ucAppPort config) . serve api $ pure config

-- | Init DB with migrations.
initDb
  :: (MonadIO m, MonadFail m, WithLog m, WithError m)
  => UnionConfig
  -> m (Maybe MigrationError)
initDb config =
  toAppError DbError (migrate (dcMigrations . ucDatabase $ config))
    `catchError` \err -> logError (showErr err) >> fail "Check logs 🠑"

-- | Create 'Env' and run 'Union' action with this environment.
runUnion :: Maybe FilePath -> Union a -> IO a
runUnion configPath action = do
  config <- maybe (pure defaultUnionConfig) loadConfig configPath
  let DatabaseConfig {..} = ucDatabase config
  let logger              = Core.setLogger $ ucSeverity config

  Core.withDb dcPoolSize dcTimeout dcCredentials $ \pool -> do
    let env = Env { eConfig = config, eDbPool = pool, eLogger = logger }
    liftIO $ Core.runApp env action
