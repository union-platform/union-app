-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Mock monad for testing purposes.
module Test.Mock
  ( MockApp
  , mockEnv
  , runMockApp
  , initMockApp
  , withClient
  , rootClient
  , apiStatusCode
  ) where

import Relude

import Control.Exception (throwIO)
import Data.Default (def)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (Status)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.Client
  ( BaseUrl(..)
  , ClientError(..)
  , ClientM
  , mkClientEnv
  , parseBaseUrl
  , responseStatusCode
  , runClientM
  )
import Servant.Client.Core (RunClient)
import Servant.Client.Generic (AsClientT, genericClient)
import Servant.Server (Application, serve)
import System.Process (readProcess)

import qualified Core

import Core.Db (DbCredentials(..))
import Core.Jwt (JwtSecret(..))
import Core.Logger (emptyLogger)
import Core.Monad (runApp)
import Core.Sender (AuthToken(..), SenderAccount(..))
import Union.App.Configuration (Config(..), DatabaseConfig(..), loadConfig)
import Union.App.Db (runDb)
import Union.App.Env (Env(..))
import Union.App.Error (Error)
import Union.Server (Endpoints, api, server)


-- | Mock monad.
type MockApp = Core.App Error Env

-- | Helper to generate temporary DB connection string.
generateTempDb :: IO String
generateTempDb = readProcess "sh" ["-c", "./tests/setup-db.sh"] []

-- | Create Union 'Env' from test config, but with temp db, created by
-- separate script.
mockEnv :: IO Env
mockEnv = do
  cfg <- maybe (pure def) loadConfig (Just "./tests/config.yaml")
  db  <- DbCredentials . toText <$> generateTempDb
  let eConfig = cfg { cDatabase = (cDatabase cfg) { dcCredentials = db } }
  let DatabaseConfig {..} = cDatabase eConfig
  let eLogger             = emptyLogger
  let eJwtSecret          = JwtSecret "0123456789"
  let eSenderService = (SenderAccount "0123456789", AuthToken "0123456789")
  Core.withDb dcPoolSize dcTimeout dcCredentials $ \eDbPool -> pure Env { .. }

-- | 'MockApp' runner.
runMockApp :: Env -> MockApp a -> IO a
runMockApp = runApp

-- | Initialize 'MockApp'.
initMockApp :: Env -> IO ()
initMockApp env = runApp env $ do
  Config {..} <- Core.grab @Config
  void . runDb . Core.migrate $ dcMigrations cDatabase

mockApplication :: Env -> Application
mockApplication = serve api . server

-- | Creating a new Manager is a relatively expensive operation, so we will try
-- share a single Manager between tests.
withClient :: Env -> ClientM a -> IO a
withClient env action = do
  runApp env
    . liftIO
    . testWithApplication (pure $ mockApplication env)
    $ \port -> do
        baseUrl <- parseBaseUrl "http://localhost"
        manager <- newManager defaultManagerSettings
        let clientEnv = mkClientEnv manager (baseUrl { baseUrlPort = port })
        runClientM action clientEnv >>= either throwIO pure

-- | Represents our API in terms of client.
rootClient :: RunClient m => Endpoints (AsClientT m)
rootClient = genericClient

-- | Checks that returned 'Status' is expected.
apiStatusCode :: Status -> ClientError -> Bool
apiStatusCode status servantError = case servantError of
  FailureResponse _ response -> responseStatusCode response == status
  _                          -> False
