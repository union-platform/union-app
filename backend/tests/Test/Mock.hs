-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Mock monad for testing purposes.
module Test.Mock
  ( MockApp
  , runMockApp
  ) where

import Relude

import Core.Has (Has(..), grab)
import Core.Jwt
  (JwtSecret(..), MonadJwt(..), decodeIntIdPayload, encodeIntIdPayload,
  mkJwtTokenImpl, verifyJwtTokenImpl)
import Core.Monad (App, runApp)


-- | Mock monad.
type MockApp = App () MockEnv

-- | Environment for 'MockApp'.
newtype MockEnv = MockEnv
  { mockEnvJwtSecret :: JwtSecret
  }

instance Has JwtSecret MockEnv where
  obtain = mockEnvJwtSecret

instance MonadJwt Int MockApp where
  mkJwtToken expiry payload = do
    secret <- grab @JwtSecret
    mkJwtTokenImpl encodeIntIdPayload secret expiry payload

  verifyJwtToken token = do
    secret <- grab @JwtSecret
    verifyJwtTokenImpl decodeIntIdPayload secret token

mockEnv :: MockEnv
mockEnv = MockEnv
  { mockEnvJwtSecret = JwtSecret "0123456789"
  }

runMockApp :: MockApp a -> IO a
runMockApp = runApp mockEnv
