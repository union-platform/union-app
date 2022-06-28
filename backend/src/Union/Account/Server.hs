-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents account related endpoints and its handlers.
module Union.Account.Server
  ( AccountAPI
  , AccountEndpoints(..)
  , accountEndpoints
  ) where

import Relude

import Network.Socket (SockAddr)
import Servant ((:>), JSON, Post, ReqBody)
import Servant.API
  (Description, Header, NoContent(..), PostCreated, RemoteHost, Summary)
import Servant.API.Generic ((:-), ToServantApi)

import Core.Error (throwError, throwOnNothingM)
import Core.Logger (Severity(..), logDebug, logInfo)
import Core.Sender (isPhoneValid)

import Union.Account.Schema (ConfirmationScope(..), UserAgent)
import Union.Account.Service
  (createAccount, findAccount, sendConfirmationCode, signIn)
import Union.Account.Types (RequestCodeReq(..), SignInReq(..), SignInResp(..))
import Union.App.Env (Union, WithError, WithJwt, WithLog, WithSender)
import Union.App.Error (Error(..))


-- | Helper type to represent Account API in terms of Servant.
type AccountAPI = ToServantApi AccountEndpoints

-- | Represents API related to account.
data AccountEndpoints route = AccountEndpoints
  { _requestCode :: route
      :- "accounts"
      :> "signIn"
      :> "request"
      :> Summary "Request code to Sign In"
      :> Description
        "First step in Sign In process. At this step user requests confirmation \
        \code, which we will send to provided phone. If there is no account with\
        \ such phone - we will create it and send code then."
      :> ReqBody '[JSON] RequestCodeReq
      :> PostCreated '[JSON] NoContent
  , _signIn :: route
      :- "accounts"
      :> "signIn"
      :> Summary "Sign In to account"
      :> Description
        "Second step in Sign In process. At this step user confirms that account\
        \ belongs to him - we expect phone number and confirmation code, which \
        \was sent at previous step. As a result - user will get JWT token."
      :> RemoteHost
      :> Header "User-Agent" UserAgent
      :> ReqBody '[JSON] SignInReq
      :> Post '[JSON] SignInResp
  } deriving stock (Generic)

-- | Endpoints related to account.
accountEndpoints :: AccountEndpoints Union
accountEndpoints = AccountEndpoints
  { _requestCode = requestCodeHandler
  , _signIn      = signInHandler
  }


-- | Handler for sign in first step: when confirmation code is requested.
requestCodeHandler
  :: (MonadIO m, WithLog m, WithError m, WithSender m)
  => RequestCodeReq
  -> m NoContent
requestCodeHandler RequestCodeReq {..} = do
  unless (isPhoneValid rc_reqPhone) . throwError Info $ BadRequest
    "Provided phone is not valid"
  logInfo $ show rc_reqPhone <> " requested OTP to sign in"
  account <- whenNothingM (findAccount rc_reqPhone) $ do
    logDebug $ "Account " <> show rc_reqPhone <> " was not found, creating"
    createAccount rc_reqPhone
  void $ sendConfirmationCode SignIn account
  pure NoContent

-- | Handler for sign in first step: when confirmation code is provided.
signInHandler
  :: (MonadIO m, WithLog m, WithError m, WithJwt m, WithSender m)
  => SockAddr
  -- ^ Request address to record 'AuthLog'
  -> Maybe UserAgent
  -> SignInReq
  -> m SignInResp
signInHandler address agent SignInReq {..} = do
  logDebug
    $  show si_reqPhone
    <> " attempts to sign in with OTP "
    <> show si_reqCode
  account <-
    throwOnNothingM
        Info
        (NotAllowed "Provided phone number or OTP is not valid")
      $ findAccount si_reqPhone
  -- account <-
    -- whenNothingM (findAccount si_reqPhone) . throwError Info $ NotAllowed
      -- "Provided phone number or OTP is not valid"
  logInfo $ "Account " <> show si_reqPhone <> " signed in"
  -- if code is not valid `signIn` will throw same error as we do it in case
  -- when there is no such number
  SignInResp <$> signIn account si_reqCode address agent
