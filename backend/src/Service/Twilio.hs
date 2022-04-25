-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module provides tools to work with external SMS provider.
module Service.Twilio
  ( findAuthToken
  , findSenderAccount
  , sendSms
  ) where

import Relude

import Data.Aeson (FromJSON, ToJSON)
import Deriving.Aeson (CustomJSON(..))
import Network.HTTP.Req
  ((/:), (=:), POST(..), ReqBodyUrlEnc(..), Scheme(..), Url, https)

import qualified Data.ByteString.Base64 as BS64
import qualified Network.HTTP.Req as HTTP

import Core.Json (JsonSnakeCase)
import Core.Sender
  (AuthToken(..), ConfirmationCode(..), Phone(..), SenderAccount(..))


-- | Env name for Twilio 'AuthToken'.
envSenderAccount :: String
envSenderAccount = "UNION_TWILIO_SENDER_ACCOUNT"

-- | Finds Twilio 'AuthToken' in env variables.
findSenderAccount :: (MonadIO m, MonadFail m) => m SenderAccount
findSenderAccount =
  lookupEnv envSenderAccount <&> fmap (SenderAccount . toText) >>= \case
    Just account -> pure account
    Nothing ->
      fail $ "Cannot start, make sure " <> envSenderAccount <> " is set"
{-# INLINE findSenderAccount #-}

-- | Env name for Twilio 'AuthToken'.
envAuthToken :: String
envAuthToken = "UNION_TWILIO_AUTH_TOKEN"

-- | Finds Twilio 'AuthToken' in env variables.
findAuthToken :: (MonadIO m, MonadFail m) => m AuthToken
findAuthToken = lookupEnv envAuthToken <&> fmap (AuthToken . toText) >>= \case
  Just token -> pure token
  Nothing    -> fail $ "Cannot start, make sure " <> envAuthToken <> " is set"
{-# INLINE findAuthToken #-}


-- | Represents response from request to send SMS via Twilio.
-- Note: we do not define all fields, because we do not need it now.
-- For now we only collect info for error case to debug it easier.
data TwilioResponse = TwilioResponse
  { trSid          :: Text
  , trErrorCode    :: Maybe Text
  , trErrorMessage :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON (JsonSnakeCase "tr") TwilioResponse

-- | Builds URL for request to Twilio.
buildUrl :: SenderAccount -> Url 'Https
buildUrl (SenderAccount aSID) =
  https "api.twilio.com"
    /: "2010-04-01"
    /: "Accounts"
    /: aSID
    /: "Messages.json"
{-# INLINE buildUrl #-}

-- | Builds message body.
buildMessage :: Phone -> Phone -> ConfirmationCode -> ReqBodyUrlEnc
buildMessage from to code = ReqBodyUrlEnc
  ("Body" =: body <> "From" =: getPhone from <> "To" =: getPhone to)
  where body = "Your confirmation code is " <> getCode code
{-# INLINE buildMessage #-}

-- | Builds headers, required for Twilio request.
buildHeaders :: SenderAccount -> AuthToken -> HTTP.Option scheme
buildHeaders (SenderAccount aSID) (AuthToken token) =
  HTTP.header "Authorization" $ "Basic " <> secret
  where secret = BS64.encode . encodeUtf8 $ aSID <> ":" <> token
{-# INLINE buildHeaders #-}

-- | Builds textual representation of error.s
buildError :: TwilioResponse -> Text
buildError TwilioResponse {..} =
  trSid
    <> ":"
    <> fromMaybe "_" trErrorCode
    <> " "
    <> fromMaybe "" trErrorMessage
{-# INLINE buildError #-}

-- | Sends SMS code via Twilio.
sendSms
  :: MonadIO m
  => SenderAccount
  -> AuthToken
  -> Phone
  -> Phone
  -> ConfirmationCode
  -> m (Either Text ())
sendSms account token from to code = HTTP.runReq HTTP.defaultHttpConfig $ do
  let msg     = buildMessage from to code
  let headers = buildHeaders account token
  v <- HTTP.req POST (buildUrl account) msg HTTP.jsonResponse headers
  let resp@TwilioResponse {..} = HTTP.responseBody v
  if isJust trErrorCode || isJust trErrorMessage
    then pure $ Left $ buildError resp
    else pure $ Right ()
