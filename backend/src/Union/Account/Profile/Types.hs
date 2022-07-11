-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents profile related types, which is not related to DB,
-- mostly here is helper types for API.
module Union.Account.Profile.Types
  ( CreateProfileReq(..)
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
