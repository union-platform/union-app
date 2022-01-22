-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Mock monad for testing purposes.
module Test.Mock
  ( MockApp
  , runMockApp
  ) where

import Relude

import qualified Core

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


-- | Mock monad.
type MockApp = App () MockEnv

-- | Environment for 'MockApp'.
newtype MockEnv = MockEnv { meJwtSecret :: JwtSecret }
  deriving (Has JwtSecret) via Core.Field "meJwtSecret" MockEnv

instance MonadJwt Int MockApp where
  mkJwtToken expiry payload = do
    secret <- Core.grab @JwtSecret
    mkJwtTokenImpl encodeIntIdPayload secret expiry payload

  verifyJwtToken token = do
    secret <- Core.grab @JwtSecret
    verifyJwtTokenImpl decodeIntIdPayload secret token

mockEnv :: MockEnv
mockEnv = MockEnv { meJwtSecret = JwtSecret "0123456789" }

runMockApp :: MockApp a -> IO a
runMockApp = runApp mockEnv
