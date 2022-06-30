-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents profile related types, which is not related to DB,
-- mostly here is helper types for API.
module Union.Account.Profile.Types
  ( UserName
  , mkUserName
  , CreateProfileReq(..)
  ) where

import Relude

import qualified Data.OpenApi as O
import qualified Data.Text as T

import Control.Lens ((?~))
import Control.Lens.Setter (mapped)
import Data.Aeson (FromJSON, ToJSON(..))
import Data.OpenApi (ToParamSchema, ToSchema(..))
import Deriving.Aeson (CustomJSON(..))
import Rel8 (DBEq, DBType)
import Servant.API (FromHttpApiData, ToHttpApiData)
import Text.Regex (matchRegex, mkRegex)

import Core.Json (JsonCamelCase)
import Core.Swagger (genericNamedSchema)


-- | Represents user name.
newtype UserName = UserName
  { getUserName :: Text
  }
  deriving stock Generic
  deriving newtype
    ( Show, Eq, DBType, DBEq, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData
    , ToParamSchema, ToSchema
    )

-- | Creates 'UserName' if provided input is valid.
mkUserName :: Text -> Maybe UserName
mkUserName (T.strip -> name) = case matchRegex r $ toString name of
  Nothing -> Nothing
  Just _  -> Just $ UserName name
  where r = mkRegex "^[a-zA-Z ]{4,32}$"


-- | Create Profile request.
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
