-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Mock monad for testing purposes.
module Test.Mock
  ( MockApp
  , MockEnv
  , mockEnv
  , runMockApp
  ) where

import Relude

import System.Process (readProcess)

import qualified Core

import Core.Db (DbPool)
import Core.Has (Has)
import Core.Jwt
  ( JwtSecret(..)
  , MonadJwt(..)
  , decodeIntIdPayload
  , encodeIntIdPayload
  , mkJwtTokenImpl
  , verifyJwtTokenImpl
  )
import Core.Monad (App, runApp)
import Union.App.Configuration
  (Config(..), DatabaseConfig(..), defaultConfig, loadConfig)


-- | Mock monad.
type MockApp = App () MockEnv

-- | Environment for 'MockApp'.
data MockEnv = MockEnv
  { meConfig    :: !Config
  , meJwtSecret :: JwtSecret
  , meDbPool    :: !DbPool
  }
  deriving (Has Config)    via Core.Field "meConfig"    MockEnv
  deriving (Has JwtSecret) via Core.Field "meJwtSecret" MockEnv
  deriving (Has DbPool)    via Core.Field "meDbPool"    MockEnv

instance MonadJwt Int MockApp where
  mkJwtToken expiry payload = do
    secret <- Core.grab @JwtSecret
    mkJwtTokenImpl encodeIntIdPayload secret expiry payload

  verifyJwtToken token = do
    secret <- Core.grab @JwtSecret
    verifyJwtTokenImpl decodeIntIdPayload secret token

-- | Create Union 'MockEnv' from test config, but with temp db, created by
-- separate script.
mockEnv :: IO MockEnv
mockEnv = do
  cfg <- maybe (pure defaultConfig) loadConfig (Just "./tests/config.yaml")
  db  <- readProcess "sh" ["-c", "./tests/setup-db.sh"] []
  let
    meConfig =
      cfg { cDatabase = (cDatabase cfg) { dcCredentials = toText db } }
  let meJwtSecret         = JwtSecret "0123456789"
  let DatabaseConfig {..} = cDatabase meConfig
  Core.withDb dcPoolSize dcTimeout dcCredentials
    $ \meDbPool -> pure MockEnv { .. }

-- | 'MockApp' runner.
runMockApp :: MockEnv -> MockApp a -> IO a
runMockApp = runApp
