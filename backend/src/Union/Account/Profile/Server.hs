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
  (Description, NamedRoutes, NoContent(..), PostCreated, Put, Summary)
import Servant.API.Generic ((:-))

import Core.Error (throwError, throwOnNothing)
import Core.Logger (Severity(..))

import Union.Account.Profile.Service (createProfile, findProfile)
import Union.Account.Profile.Types
  (CreateInterestReq(..), CreateProfileReq(..), UpdateInterestsReq(..))
import Union.Account.Schema (AccountId)
import Union.Account.Types (mkUserName)
import Union.App.Env (Union, WithDb, WithError, WithLog)
import Union.App.Error (Error(..))


-- | Helper type to represent Profile API in terms of Servant.
type ProfileAPI = NamedRoutes ProfileEndpoints

-- | Represents API related to profile.
data ProfileEndpoints mode = ProfileEndpoints
  { _createProfile :: mode
      :- "profile"
      :> Summary "Create profile"
      :> Description
        "With this endpoint we can create actual user profile. Account stores \
        \information required only for authentication and some meta info, but \
        \in profile we store actual information about user."
      :> ReqBody '[JSON] CreateProfileReq
      :> PostCreated '[JSON] NoContent
  , _createInterest :: mode
      :- "profile"
      :> "interests"
      :> Summary "Create interest"
      :> Description
        "Create new interest. It's not related to any account yet, but creates \
        \new entry in database. We need it now to collect set of interests and \
        \most likely we will delete this endpoint in future and allow to pick \
        \from existing only."
      :> ReqBody '[JSON] CreateInterestReq
      :> PostCreated '[JSON] NoContent
  , _updateInterests :: mode
      :- "profile"
      :> "interests"
      :> Summary "Update interests"
      :> Description
        "Update user interests - whether he had it before or not you can use \
        \this endpoint."
      :> ReqBody '[JSON] UpdateInterestsReq
      :> Put '[JSON] NoContent
  } deriving stock (Generic)

-- | Endpoints related to profile.
profileEndpoints :: AccountId -> ProfileEndpoints Union
profileEndpoints aId = ProfileEndpoints
  { _createProfile   = createProfileHandler aId
  , _createInterest  = createInterestHandler
  , _updateInterests = updateInterestsHandler aId
  }


-- | Handler to create 'Profile'.
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

-- | Handler to create 'Interest'.
createInterestHandler :: Monad m => CreateInterestReq -> m NoContent
createInterestHandler CreateInterestReq{} = do
  pure NoContent


-- | Handler to update 'InterestMap'.
updateInterestsHandler
  :: Monad m => AccountId -> UpdateInterestsReq -> m NoContent
updateInterestsHandler _aId UpdateInterestsReq{} = do
  pure NoContent
