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

  -- * API protection
  , authProxy
  , authCtx
  , Protected
  , protected
  ) where

import Relude

import Control.Lens ((.~), (?~), At(..), Iso', coerced)
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi
  ( Definitions
  , HasComponents(..)
  , HasDescription(..)
  , HasSecurity(..)
  , HasSecuritySchemes(..)
  , HttpSchemeType(..)
  , SecurityDefinitions(..)
  , SecurityRequirement(..)
  , SecurityScheme(..)
  , SecuritySchemeType(..)
  , ToParamSchema
  , ToSchema
  , allOperations
  , setResponse
  )
import Data.Time (getCurrentTime)
import Servant.API (type (:>), FromHttpApiData)
import Servant.Auth.Server
  ( Auth
  , AuthResult(..)
  , CookieSettings
  , JWT
  , JWTSettings(..)
  , defaultCookieSettings
  , generateKey
  , makeJWT
  )
import Servant.OpenApi (HasOpenApi(..))
import Servant.Server (Context(..))

import qualified Core

import Core.Error (ThrowAll(..))
import Core.Logger (Severity(..))

import Union.Account.Schema (AccountId)
import Union.App.Configuration (Config(..))
import Union.App.Env (Env(..), WithEnv, WithError)
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

-- | Proxy required for 'hoistServerWithContext' to protect API.
authProxy :: Proxy '[CookieSettings , JWTSettings]
authProxy = Proxy
{-# INLINE authProxy #-}

-- | Context required for 'serveWithContext' to protect API.
authCtx :: Env -> Context '[CookieSettings, JWTSettings]
authCtx env = defaultCookieSettings :. eJwtSettings env :. EmptyContext
{-# INLINE authCtx #-}

-- | Helper type to mark API is protected with authentication.
type Protected = Auth '[JWT] AccountId

-- | For the endpoints which actually require authentication, checks whether
-- the request provides a valid authentication token.
-- Otherwise it returns a 401 response
protected
  :: ThrowAll Error handler
  => (AccountId -> handler)
  -> AuthResult AccountId
  -> handler
protected handler = \case
  (Authenticated aId) -> handler aId
  _                   -> throwAll Info $ NotAllowed "Authorization failed"

instance HasOpenApi api => HasOpenApi (Protected :> api) where
  toOpenApi _ =
    toOpenApi @api Proxy
      & (components . securitySchemes . securityDefinitions . at "JWT" ?~ idJWT)
      & (  allOperations
        .  security
        .~ [SecurityRequirement $ mempty & at "JWT" ?~ []]
        )
      & setResponse 401 (pure $ mempty & description .~ "Authorization failed")
    where
      idJWT = SecurityScheme
        (SecuritySchemeHttp . HttpSchemeBearer $ Just "jwt")
        (Just "JSON Web Token-based API key")

securityDefinitions :: Iso' SecurityDefinitions (Definitions SecurityScheme)
securityDefinitions = coerced
{-# INLINE securityDefinitions #-}
