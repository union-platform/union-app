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
  , WithSender
  ) where

import Relude

import Data.Default (def)
import Network.HTTP.Req (HttpException(..))
import Servant.Server.Generic (AsServerT)
import UnliftIO.Exception (catch)

import qualified Core
import qualified Service.Twilio as Twilio

import Core.Db (DbPool)
import Core.Has (Has)
import Core.Jwt (JwtPayload, JwtSecret(..), JwtToken, MonadJwt)
import Core.Logger (Log, Logger, Severity(..), logError, logException)
import Core.Sender
  (AuthToken, ConfirmationCode, MonadSender(..), Phone, SenderAccount)
import Core.Time (Seconds)

import Union.App.Configuration (Config(..), DatabaseConfig(..), loadConfig)
import Union.App.Error (Error(..))


-- | Main application monad.
type App = Core.App Error Env

-- | Union application server.
type Union = AsServerT App

-- | Constraint to actions with DB access.
type WithDb m = Core.WithDb Env m

-- | Constraint to actions with logging.
type WithLog m = Core.WithLog Env m

-- | Constraint to actions that can throw and catch pure errors with call-stack.
type WithError m = Core.WithError Error m

-- | Constraint to actions with Jwt generation / verification.
type WithJwt m = MonadJwt Int64 m

-- | Constraint to actions with Sender service.
type WithSender m = MonadSender m


-- | Helper to simplify deriving for 'Env'.
type EnvField f = Core.Field f Env

-- | Union environment.
data Env = Env
  { eConfig        :: !Config
  , eDbPool        :: !DbPool
  , eLogger        :: !(Logger App)
  , eJwtSecret     :: !JwtSecret
  , eSenderService :: !(SenderAccount, AuthToken)
    -- ^ Stores Twilio account and auth token.
  }
  deriving (Has Config)                     via EnvField "eConfig"
  deriving (Has DbPool)                     via EnvField "eDbPool"
  deriving (Has JwtSecret)                  via EnvField "eJwtSecret"
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

-- | Instance to specify how to generate and send 'ConfirmationCode'.
instance MonadSender App where
  sendCode :: HasCallStack => Phone -> ConfirmationCode -> App ()
  sendCode to code = do
    (account, token) <- Core.grab @(SenderAccount, AuthToken)
    cSenderPhone <$> Core.grab @Config >>= \case
      Just senderPhone -> do
        result <- Twilio.sendSms account token senderPhone to code `catch` \case
          VanillaHttpException e -> do
            logException e >> Core.throwError
              Error
              (ApiError "Cannot send confirmation code")
          JsonHttpException e -> Core.throwError Error . ApiError $ toText e
        case result of
          Left  e -> Core.throwError Error $ ApiError e
          Right _ -> pass
      Nothing -> pass

-- | Create Union 'Env' with given path to 'Config'.
buildEnv :: Maybe FilePath -> IO Env
buildEnv path = do
  eConfig <- maybe (pure def) loadConfig path
  let DatabaseConfig {..} = cDatabase eConfig
  let eLogger             = Core.setLogger $ cSeverity eConfig
  eJwtSecret    <- JwtSecret <$> Core.mkRandomString 10
  senderAccount <- Twilio.findSenderAccount
  senderToken   <- Twilio.findAuthToken
  let eSenderService = (senderAccount, senderToken)
  Core.withDb dcPoolSize dcTimeout dcCredentials $ \eDbPool -> pure Env { .. }

-- | Runs provided action with new 'Env'.
runWithEnv :: Maybe FilePath -> App a -> IO a
runWithEnv path action = buildEnv path >>= flip Core.runApp action
{-# INLINE runWithEnv #-}

-- | Helper to kill the app.
kill :: (MonadReader env m, MonadFail m, WithLog m) => Text -> m b
kill msg = withFrozenCallStack (logError msg >> fail "Check logs ⇡")
{-# INLINE kill #-}
