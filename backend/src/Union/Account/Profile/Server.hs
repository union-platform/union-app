-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents profile related endpoints and its handlers.
module Union.Account.Profile.Server
  ( ProfileAPI
  , ProfileEndpoints(..)
  , profileEndpoints
  ) where

import Relude

import Servant ((:>), JSON, ReqBody)
import Servant.API
  (Description, NamedRoutes, NoContent(..), PostCreated, Summary)
import Servant.API.Generic ((:-))

import Core.Error (throwOnNothing)
import Core.Logger (Severity(..))

import Union.Account.Profile.Types (CreateProfileReq(..), mkUserName)
import Union.App.Env (Union, WithError, WithLog)
import Union.App.Error (Error(..))


-- | Helper type to represent Profile API in terms of Servant.
type ProfileAPI = NamedRoutes ProfileEndpoints

-- | Represents API related to profile.
newtype ProfileEndpoints mode = ProfileEndpoints
  { _createProfile :: mode
      :- "accounts"
      :> "profile"
      :> Summary "Create profile"
      :> Description
        "With this endpoint we can create actual user profile. Account stores \
        \information required only for authentication and some meta info, but \
        \in profile we store actual information about user."
      :> ReqBody '[JSON] CreateProfileReq
      :> PostCreated '[JSON] NoContent
  } deriving stock (Generic)

-- | Endpoints related to profile.
profileEndpoints :: ProfileEndpoints Union
profileEndpoints = ProfileEndpoints { _createProfile = createProfileHandler }


-- | Handler to create profile.
createProfileHandler
  :: (WithLog m, WithError m) => CreateProfileReq -> m NoContent
createProfileHandler CreateProfileReq {..} = do
  _name <- throwOnNothing Info (BadRequest "Provided name is not valid")
    $ mkUserName cp_reqName
  -- TODO: add actual service
  pure NoContent
