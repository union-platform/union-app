-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents signIn related types, which is not related to DB,
-- mostly here is helper types for API.
module Union.Account.SignIn.Types
  ( RequestCodeReq(..)
  , UserAgent(..)
  , AuthenticateReq(..)
  , AuthenticateResp(..)
  ) where

import Relude

import qualified Data.OpenApi as O

import Control.Lens ((?~))
import Control.Lens.Setter (mapped)
import Data.Aeson (FromJSON, ToJSON(..))
import Data.OpenApi (ToParamSchema, ToSchema(..))
import Deriving.Aeson (CustomJSON(..))
import Rel8 (DBEq, DBType)
import Servant.API (FromHttpApiData, ToHttpApiData)

import Core.Json (JsonCamelCase)
import Core.Sender (ConfirmationCode(..), Phone(..))
import Core.Swagger (genericNamedSchema)

import Union.Auth (JwtToken)


-- | Request Code request (Sign In 1 step).
newtype RequestCodeReq = RequestCodeReq
  { rc_reqPhone :: Phone
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON (JsonCamelCase "rc_req") RequestCodeReq

instance ToSchema RequestCodeReq where
  declareNamedSchema = genericNamedSchema @(JsonCamelCase "rc_req") extra
    where
      extra =
        [ mapped . O.schema . O.example ?~ toJSON
            (RequestCodeReq $ Phone "+12345678901")
        ]

-- | Represents 'User-Agent' header.
newtype UserAgent = UserAgent
  { getUserAgent :: Text
  }
  deriving stock Generic
  deriving newtype
    ( Show, Eq, DBType, DBEq, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData
    , ToParamSchema, ToSchema
    )

-- | Sign In request (Sign In 2 step).
data AuthenticateReq = AuthenticateReq
  { a_reqPhone :: Phone
  , a_reqCode  :: ConfirmationCode
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON
    (JsonCamelCase "a_req")
    AuthenticateReq

instance ToSchema AuthenticateReq where
  declareNamedSchema = genericNamedSchema @(JsonCamelCase "a_req") extra
    where
      extra =
        [ mapped . O.schema . O.example ?~ toJSON
          (AuthenticateReq (Phone "+12345678901") (ConfirmationCode "123456"))
        , mapped
          .  O.schema
          .  O.description
          ?~ "Request to receive authentication token, here we expect code that "
          <> "was sended via separate endpoint for such requests"
        ]

-- | Sign In response.
newtype AuthenticateResp = AuthenticateResp
  { a_respToken :: JwtToken
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON (JsonCamelCase "a_resp") AuthenticateResp

instance ToSchema AuthenticateResp where
  declareNamedSchema = genericNamedSchema @(JsonCamelCase "a_resp") extra
    where
      extra =
        [ mapped
            .  O.schema
            .  O.description
            ?~ "Returns JWT token for further use as authenticated user."
        ]
