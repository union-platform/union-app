-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: MPL-2.0

-- | This module provides convenient wrappers around JWT provided by @jwt@
-- Haskell library. These tokens are used to authenticate users and store
-- additional payload on user log in, for example, user id.
module Core.Jwt
  ( -- * Secret
    JwtSecret(..)

    -- * Token and payloads
    -- ** Data types
  , JwtToken(..)
  , JwtPayload(..)
    -- ** Coders
  , encodeIntIdPayload
  , decodeIntIdPayload
  , encodeTextIdPayload
  , decodeTextIdPayload
    -- ** Helpers to write coders
  , payloadToMap
  , payloadFromMap

    -- * Jwt Effect
    -- ** Monad
  , MonadJwt(..)
    -- ** Internals of 'MonadJwt'
  , mkJwtTokenImpl
  , verifyJwtTokenImpl
  ) where

import Relude

import qualified Data.Aeson as Json
import qualified Data.Map as Map
import qualified Web.JWT as Jwt

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Scientific (toBoundedInteger)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Web.HttpApiData (FromHttpApiData)

import Core.Time (Seconds(..))


-- | JWT secret that is used to sign and verify JSON web tokens.
-- You can use functions from "Core.Random" module to create random
-- 'JwtSecret' at the start of your application like this:
-- @
-- jwtSecret <- 'JwtSecret' \<$\> mkRandomString 10
-- @
newtype JwtSecret = JwtSecret { unJwtSecret :: Text }

-- | Encoded JSON web token.
newtype JwtToken = JwtToken { unJwtToken :: Text }
  deriving newtype (Eq, Ord, Hashable, FromHttpApiData, FromJSON, ToJSON)
  deriving stock (Show, Generic)

-- | Stores arbitrary payload. If you want to store your custom payload, you
-- need to specify two functions:
-- 1. How to encode 'JwtPayload' as 'Jwt.ClaimsMap'.
-- 2. How to decode 'JwtPayload' from 'Jwt.ClaimsMap'.
--
-- See examples in this module: 'encodeIntIdPayload', 'decodeIntIdPayload',
-- 'encodeTextIdPayload', 'decodeTextIdPayload'.
newtype JwtPayload a = JwtPayload { unJwtPayload :: a }
  deriving newtype (Eq)
  deriving stock (Show, Functor)

-- | Encodes 'JwtPayload' that stores 'Int' as payload with name @id@. Use it if
-- you store ids as integer values. Dual to 'decodeIntIdPayload'.
encodeIntIdPayload :: JwtPayload Int -> Jwt.ClaimsMap
encodeIntIdPayload = payloadToMap . Json.Number . fromIntegral . unJwtPayload
{-# INLINE encodeIntIdPayload #-}

-- | Decodes 'JwtPayload' from 'Jwt.ClaimsMap' that stores 'Int' under name
-- @id@. Dual to 'encodeIntIdPayload'.
decodeIntIdPayload :: Jwt.ClaimsMap -> Maybe (JwtPayload Int)
decodeIntIdPayload = fmap JwtPayload . payloadFromMap
  (\case
    Json.Number jwtId -> toBoundedInteger jwtId
    _                 -> Nothing
  )
{-# INLINE decodeIntIdPayload #-}

-- | Encodes 'JwtPayload' that stores 'Text' as payload with name @id@. Use it if
-- you store ids as text or UUID values. Dual to 'decodeTextIdPayload'.
encodeTextIdPayload :: JwtPayload Text -> Jwt.ClaimsMap
encodeTextIdPayload = payloadToMap . Json.String . unJwtPayload
{-# INLINE encodeTextIdPayload #-}

-- | Decodes 'JwtPayload' from 'Jwt.ClaimsMap' that stores 'Int' under name
-- @id@. Dual to 'encodeIntIdPayload'.
decodeTextIdPayload :: Jwt.ClaimsMap -> Maybe (JwtPayload Text)
decodeTextIdPayload = fmap JwtPayload . payloadFromMap
  (\case
    Json.String jwtId -> Just jwtId
    _                 -> Nothing
  )
{-# INLINE decodeTextIdPayload #-}

-- | Creates 'Jwt.ClaimsMap' from 'Json.Value' under name @id@.
payloadToMap :: Json.Value -> Jwt.ClaimsMap
payloadToMap val = Jwt.ClaimsMap $ Map.fromList [("id", val)]
{-# INLINE payloadToMap #-}

-- | Decodes payload from 'Jwt.ClaimsMap' under @id@ name given 'Json.Value'
-- extractor.
payloadFromMap :: (Value -> Maybe a) -> Jwt.ClaimsMap -> Maybe a
payloadFromMap fromValue (Jwt.ClaimsMap claimsMap) =
  Map.lookup "id" claimsMap >>= fromValue
{-# INLINE payloadFromMap #-}


-- | This monad represents effect to create and verify JWT. It has two type
-- variables:
-- * @payload@: type of JWT payload.
-- * @m@: monad itself
class Monad m => MonadJwt payload m where
  mkJwtToken
    :: Seconds
    -- ^ Token expiry in seconds
    -> JwtPayload payload
    -- ^ Payload to code
    -> m JwtToken
    -- ^ Encoded token

  verifyJwtToken
    :: JwtToken
    -- ^ Token which stores payload
    -> m (Maybe (JwtPayload payload))
    -- ^ Decoded payload if token valid

-- | Default implementation of token creation.
mkJwtTokenImpl
  :: MonadIO m
  => (JwtPayload payload -> Jwt.ClaimsMap)
  -> JwtSecret
  -> Seconds
  -> JwtPayload payload
  -> m JwtToken
mkJwtTokenImpl encode (JwtSecret jwtSecret) expiry payload = do
  let secret = Jwt.hmacSecret jwtSecret
  timeNow <- liftIO getPOSIXTime
  let expiryTime = timeNow + fromIntegral (unSeconds expiry)
  let
    claimsSet = mempty
      { Jwt.exp                = Jwt.numericDate expiryTime
      , Jwt.unregisteredClaims = encode payload
      }
  pure $ JwtToken $ Jwt.encodeSigned secret mempty claimsSet

-- | Default implementation of token validation.
verifyJwtTokenImpl
  :: MonadIO m
  => (Jwt.ClaimsMap -> Maybe (JwtPayload payload))
  -> JwtSecret
  -> JwtToken
  -> m (Maybe (JwtPayload payload))
verifyJwtTokenImpl decode (JwtSecret jwtSecret) (JwtToken token) = do
  let secret = Jwt.hmacSecret jwtSecret
  timeNow <- Jwt.numericDate <$> liftIO getPOSIXTime
  pure $ do
    claimsSet <- Jwt.claims <$> Jwt.decodeAndVerifySignature secret token
    expiryTimeStatedInToken <- Jwt.exp claimsSet
    now <- timeNow
    guard (expiryTimeStatedInToken >= now)
    decode $ Jwt.unregisteredClaims claimsSet
