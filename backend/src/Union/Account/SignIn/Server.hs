-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents signIn related endpoints and its handlers.
module Union.Account.SignIn.Server
  ( SignInAPI
  , SignInEndpoints(..)
  , signInEndpoints
  ) where

import Relude

import Network.Socket (SockAddr)
import Servant ((:>), JSON, Post, ReqBody)
import Servant.API
  ( Description
  , Header
  , NamedRoutes
  , NoContent(..)
  , PostCreated
  , RemoteHost
  , Summary
  )
import Servant.API.Generic ((:-))

import Core.Error (throwError, throwOnNothingM)
import Core.Logger (Severity(..), logDebug, logInfo)
import Core.Sender (isPhoneValid)

import Union.Account.Service (createAccount, findAccount)
import Union.Account.SignIn.Schema (ConfirmationScope(..))
import Union.Account.SignIn.Service (sendConfirmationCode, signIn)
import Union.Account.SignIn.Types
  (AuthenticateReq(..), AuthenticateResp(..), RequestCodeReq(..), UserAgent)
import Union.App.Env (Union, WithDb, WithError, WithLog, WithSender)
import Union.App.Error (Error(..))


-- | Helper type to represent SignIn API in terms of Servant.
type SignInAPI = NamedRoutes SignInEndpoints

-- | Represents API related to signIn.
data SignInEndpoints mode = SignInEndpoints
  { _requestCode :: mode
      :- "signIn"
      :> "request"
      :> Summary "Request code to Sign In"
      :> Description
        "First step in Sign In process. At this step user requests confirmation \
        \code, which we will send to provided phone. If there is no account with\
        \ such phone - we will create it and send code then."
      :> ReqBody '[JSON] RequestCodeReq
      :> PostCreated '[JSON] NoContent
  , _authenticate :: mode
      :- "signIn"
      :> Summary "Sign In to account"
      :> Description
        "Second step in Sign In process. At this step user confirms that account\
        \ belongs to him - we expect phone number and confirmation code, which \
        \was sent at previous step. As a result - user will get JWT token."
      :> RemoteHost
      :> Header "User-Agent" UserAgent
      :> ReqBody '[JSON] AuthenticateReq
      :> Post '[JSON] AuthenticateResp
  } deriving stock (Generic)

-- | Endpoints related to signIn.
signInEndpoints :: SignInEndpoints Union
signInEndpoints = SignInEndpoints
  { _requestCode  = requestCodeHandler
  , _authenticate = authenticateHandler
  }


-- | Handler for sign in first step: when confirmation code is requested.
requestCodeHandler
  :: (WithLog m, WithError m, WithSender m) => RequestCodeReq -> m NoContent
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
authenticateHandler
  :: (WithLog m, WithError m, WithDb m)
  => SockAddr
  -- ^ Request address to record 'AuthLog'
  -> Maybe UserAgent
  -> AuthenticateReq
  -> m AuthenticateResp
authenticateHandler address agent AuthenticateReq {..} = do
  logDebug
    $  show a_reqPhone
    <> " attempts to sign in with OTP "
    <> show a_reqCode
  account <-
    throwOnNothingM
        Info
        (NotAllowed "Provided phone number or OTP is not valid")
      $ findAccount a_reqPhone
  logInfo $ "Account " <> show a_reqPhone <> " signed in"
  -- if code is not valid `signIn` will throw same error as we do it in case
  -- when there is no such number
  AuthenticateResp <$> signIn account a_reqCode address agent
