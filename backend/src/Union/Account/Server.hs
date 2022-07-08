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

import Servant ((:>))
import Servant.API (NamedRoutes)
import Servant.API.Generic ((:-))

import Union.Account.Profile.Server (ProfileAPI, profileEndpoints)
import Union.Account.SignIn.Server (SignInAPI, signInEndpoints)
import Union.App.Env (Union)
import Union.Auth (Protected, protected)


-- | Helper type to represent Account API in terms of Servant.
type AccountAPI = "accounts" :> NamedRoutes AccountEndpoints

-- | Represents API related to account.
data AccountEndpoints mode = AccountEndpoints
  { _signIn  :: mode :- SignInAPI
  , _profile :: mode :- Protected :> ProfileAPI
  }
  deriving stock Generic

-- | Endpoints related to account.
accountEndpoints :: AccountEndpoints Union
accountEndpoints = AccountEndpoints
  { _signIn  = signInEndpoints
  , _profile = protected profileEndpoints
  }
