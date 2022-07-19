-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents profile related types, which is not related to DB,
-- mostly here is helper types for API.
module Union.Account.Profile.Types
  ( CreateProfileReq(..)
  , CreateInterestReq(..)
  , UpdateInterestsReq(..)
  ) where

import Relude

import qualified Data.OpenApi as O

import Control.Lens ((?~))
import Control.Lens.Setter (mapped)
import Data.Aeson (FromJSON, ToJSON(..))
import Data.OpenApi (ToSchema(..))
import Deriving.Aeson (CustomJSON(..))

import Core.Json (JsonCamelCase)
import Core.Swagger (genericNamedSchema)

import Union.Account.Profile.Schema (InterestId)


-- | Create 'Profile' request.
newtype CreateProfileReq = CreateProfileReq
  { cp_reqName :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON (JsonCamelCase "cp_req") CreateProfileReq

instance ToSchema CreateProfileReq where
  declareNamedSchema = genericNamedSchema @(JsonCamelCase "cp_req") extra
    where
      extra =
        [ mapped . O.schema . O.example ?~ toJSON
          (CreateProfileReq "Lev Tolstoy")
        , mapped . O.schema . O.description ?~ "Request to create user profile"
        ]


-- | Create 'Interest' request.
newtype CreateInterestReq = CreateInterestReq
  { ci_reqName :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON (JsonCamelCase "ci_req") CreateInterestReq

instance ToSchema CreateInterestReq where
  declareNamedSchema = genericNamedSchema @(JsonCamelCase "ci_req") extra
    where
      extra =
        [ mapped . O.schema . O.example ?~ toJSON
          (CreateInterestReq "Programming")
        , mapped
          .  O.schema
          .  O.description
          ?~ "Request to update user's interests"
        ]

-- | Update 'InterestMap' request.
newtype UpdateInterestsReq = UpdateInterestsReq
  { ui_reqInterests :: [InterestId]
  }
  deriving stock (Generic, Show, Eq)
  deriving newtype (ToJSON, FromJSON, ToSchema)
