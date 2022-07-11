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

import Core.Error (throwError, throwOnNothing)
import Core.Logger (Severity(..))

import Union.Account.Profile.Service (createProfile, findProfile)
import Union.Account.Profile.Types (CreateProfileReq(..))
import Union.Account.Schema (AccountId)
import Union.Account.Types (mkUserName)
import Union.App.Env (Union, WithDb, WithError, WithLog)
import Union.App.Error (Error(..))


-- | Helper type to represent Profile API in terms of Servant.
type ProfileAPI = NamedRoutes ProfileEndpoints

-- | Represents API related to profile.
newtype ProfileEndpoints mode = ProfileEndpoints
  { _createProfile :: mode
      :- "profile"
      :> Summary "Create profile"
      :> Description
        "With this endpoint we can create actual user profile. Account stores \
        \information required only for authentication and some meta info, but \
        \in profile we store actual information about user."
      :> ReqBody '[JSON] CreateProfileReq
      :> PostCreated '[JSON] NoContent
  } deriving stock (Generic)

-- | Endpoints related to profile.
profileEndpoints :: AccountId -> ProfileEndpoints Union
profileEndpoints aId =
  ProfileEndpoints { _createProfile = createProfileHandler aId }


-- | Handler to create profile.
createProfileHandler
  :: (WithLog m, WithError m, WithDb m)
  => AccountId
  -> CreateProfileReq
  -> m NoContent
createProfileHandler aId CreateProfileReq {..} = do
  name <- throwOnNothing Info (Invalid "Provided name is not valid")
    $ mkUserName cp_reqName
  findProfile aId >>= \case
    Just _ -> throwError Info
      $ BadRequest ("Profile for user " <> show aId <> " already exists")
    Nothing -> createProfile aId name >> pure NoContent
