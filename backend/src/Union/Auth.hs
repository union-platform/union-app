-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module provides authentication functionality.
module Union.Auth
  ( -- * JWT token
    JwtToken
  , mkJwtToken
  , unJwtToken
  , generateJwtToken
  , generateKey
  ) where

import Relude

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToParamSchema, ToSchema)
import Data.Time (getCurrentTime)
import Servant.API (FromHttpApiData)
import Servant.Auth.Server (JWTSettings(..), generateKey, makeJWT)

import qualified Core

import Core.Logger (Severity(..))

import Union.Account.Schema (AccountId)
import Union.App.Configuration (Config(..))
import Union.App.Env (WithEnv, WithError)
import Union.App.Error (Error(..))


-- | Encoded JSON web token.
newtype JwtToken = JwtToken { getJwtToken :: Text }
  deriving stock (Show, Generic)
  deriving newtype
    (Eq, Ord, FromHttpApiData, FromJSON, ToJSON, ToParamSchema, ToSchema)

-- | Creates 'JwtToken'.
mkJwtToken :: LByteString -> JwtToken
mkJwtToken = JwtToken . decodeUtf8
{-# INLINE mkJwtToken #-}

-- | Extract raw 'JwtToken'.
unJwtToken :: JwtToken -> LByteString
unJwtToken = encodeUtf8 . getJwtToken
{-# INLINE unJwtToken #-}

-- | Generates 'JwtToken' by given 'JwtPayload'.
generateJwtToken
  :: (WithError m, MonadIO m, WithEnv m) => AccountId -> m JwtToken
generateJwtToken aId = do
  currentTime <- liftIO getCurrentTime
  expire      <- cJwtExpire <$> Core.grab @Config
  let expireAt = Just $ Core.addSeconds expire currentTime
  settings <- Core.grab @JWTSettings
  liftIO (makeJWT aId settings expireAt) >>= \case
    Right token -> pure $ mkJwtToken token
    Left  e     -> Core.throwError Info . ApiError $ show e
