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
  , WithEnv
  , WithDb
  , WithLog
  , WithError
  , WithSender
  ) where

import Relude

import Data.Default (def)
import Network.HTTP.Req (HttpException(..))
import Servant.Auth.Server (JWTSettings)
import Servant.Server.Generic (AsServerT)
import UnliftIO.Exception (catch)

import qualified Core
import qualified Service.Twilio as Twilio

import Core.Db (DbCredentials(..), DbPool)
import Core.Has (Has)
import Core.Logger (Log, Logger, Severity(..), logError, logException)
import Core.Sender
  (AuthToken, ConfirmationCode, MonadSender(..), Phone, SenderAccount)

import Union.App.Configuration
  (Config(..), DatabaseConfig(..), buildJwtSettings, loadConfig)
import Union.App.Error (Error(..))


-- | Main application monad.
type App = Core.App Error Env

-- | Union application server.
type Union = AsServerT App

-- | Constraint to actions with access to 'Env'.
type WithEnv m = MonadReader Env m

-- | Constraint to actions with DB access.
type WithDb m = Core.WithDb Env m

-- | Constraint to actions with logging.
type WithLog m = Core.WithLog Env m

-- | Constraint to actions that can throw and catch pure errors with call-stack.
type WithError m = Core.WithError Error m

-- | Constraint to actions with Sender service.
type WithSender m = MonadSender m


-- | Helper to simplify deriving for 'Env'.
type EnvField f = Core.Field f Env

-- | Union environment.
data Env = Env
  { eConfig        :: !Config
  , eDbPool        :: !DbPool
  , eLogger        :: !(Logger App)
  , eJwtSettings   :: !JWTSettings
    -- ^ Stores 'JWTSettings' required for authentication.
  , eSenderService :: !(SenderAccount, AuthToken)
    -- ^ Stores Twilio account and auth token.
  }
  deriving (Has Config)                     via EnvField "eConfig"
  deriving (Has DbPool)                     via EnvField "eDbPool"
  deriving (Has JWTSettings)                via EnvField "eJwtSettings"
  deriving (Has (SenderAccount, AuthToken)) via EnvField "eSenderService"

-- | Instance to specify how to get and update the 'Logger' stored inside
-- the 'Env'.
instance Core.HasLog Env Log App where
  getLogAction :: Env -> Logger App
  getLogAction = eLogger
  {-# INLINE getLogAction #-}

  setLogAction :: Logger App -> Env -> Env
  setLogAction newLogAction env = env { eLogger = newLogAction }
  {-# INLINE setLogAction #-}

-- | Instance to specify how to generate and send 'ConfirmationCode'.
instance MonadSender App where
  sendCode :: HasCallStack => Phone -> ConfirmationCode -> App ()
  sendCode to code = do
    (account, token) <- Core.grab @(SenderAccount, AuthToken)
    cSenderPhone <$> Core.grab @Config >>= \case
      Just senderPhone -> do
        result <- Twilio.sendSms account token senderPhone to code `catch` \case
          VanillaHttpException e -> do
            logException e
            Core.throwError Error (ApiError "Cannot send confirmation code")
          JsonHttpException e -> Core.throwError Error . ApiError $ toText e
        case result of
          Left  e -> Core.throwError Error $ ApiError e
          Right _ -> pass
      Nothing -> pass

-- | Create Union 'Env' with given path to 'Config'.
buildEnv :: Maybe FilePath -> IO Env
buildEnv path = do
  cfg <- maybe (pure def) loadConfig path
  db  <- applyEnv (dcCredentials $ cDatabase cfg) <$> dbCredentialsEnv
  let eConfig = cfg { cDatabase = (cDatabase cfg) { dcCredentials = db } }
  let DatabaseConfig {..} = cDatabase eConfig
  let eLogger = Core.setLogger $ cSeverity cfg
  eJwtSettings  <- buildJwtSettings "./secret.jwk"
  senderAccount <- Twilio.findSenderAccount
  senderToken   <- Twilio.findAuthToken
  let eSenderService = (senderAccount, senderToken)
  Core.withDb dcPoolSize dcTimeout dcCredentials $ \eDbPool -> pure Env { .. }

-- | Runs provided action with new 'Env'.
runWithEnv :: Maybe FilePath -> App a -> IO a
runWithEnv path action = buildEnv path >>= flip Core.runApp action
{-# INLINE runWithEnv #-}

-- | Helper to kill the app.
kill :: (MonadFail m, WithLog m) => Text -> m b
kill msg = withFrozenCallStack (logError msg >> fail "Check logs ⇡")
{-# INLINE kill #-}


-- | Helper to build 'DbCredentials' from environment variables.
dbCredentialsEnv :: IO (Maybe DbCredentials)
dbCredentialsEnv = do
  host     <- build "host" <$> lookup envDbHost
  port     <- build "port" <$> lookup envDbPort
  user     <- build "user" <$> lookup envDbUser
  password <- build "password" <$> lookup envDbPassword
  db       <- build "dbname" <$> lookup envDbName

  if all isNothing [host, port, user, password, db]
    then pure Nothing
    else pure $ DbCredentials <$> mconcat [host, port, db, user, password]
  where
    lookup :: MonadIO m => String -> m (Maybe String)
    lookup name = lookupEnv name >>= \case
      Just env
        | null env  -> pure Nothing
        | otherwise -> pure $ Just env
      Nothing  -> pure Nothing

    build :: Text -> Maybe String -> Maybe Text
    build option env = ((" " <> option <> "=") <>) . toText <$> env

-- | Helper to combine environment variable with provided value.
applyEnv :: Semigroup a => a -> Maybe a -> a
applyEnv value = maybe value (value <>)

-- | Env name for database host.
envDbHost :: String
envDbHost = "UNION_DB_HOST"

-- | Env name for database port.
envDbPort :: String
envDbPort = "UNION_DB_PORT"

-- | Env name for database user.
envDbUser :: String
envDbUser = "UNION_DB_USER"

-- | Env name for database password.
envDbPassword :: String
envDbPassword = "UNION_DB_PASSWORD"

-- | Env name for database name.
envDbName :: String
envDbName = "UNION_DB_NAME"
