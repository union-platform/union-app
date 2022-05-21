-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents account related types, which is not related to DB,
-- mostly here is helper types for API.
module Union.Account.Types
  ( RequestCodeReq(..)
  , SignInReq(..)
  , SignInResp(..)
  ) where

import Relude

import qualified Data.OpenApi as O

import Control.Lens ((?~))
import Control.Lens.Setter (mapped)
import Data.Aeson (FromJSON, ToJSON(..))
import Data.OpenApi (ToSchema(..))
import Deriving.Aeson (CustomJSON(..))

import Core.Json (JsonCamelCase)
import Core.Jwt (JwtToken)
import Core.Sender (ConfirmationCode(..), Phone(..))
import Core.Swagger (genericNamedSchema)


-- | Sign In request.
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
            (RequestCodeReq $ Phone "+1234567890")
        ]

-- | Sign In request.
data SignInReq = SignInReq
  { si_reqPhone :: Phone
  , si_reqCode  :: ConfirmationCode
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON (JsonCamelCase "si_req") SignInReq

instance ToSchema SignInReq where
  declareNamedSchema = genericNamedSchema @(JsonCamelCase "si_req") extra
    where
      extra =
        [ mapped . O.schema . O.example ?~ toJSON
          (SignInReq (Phone "+1234567890") (ConfirmationCode "123456"))
        , mapped
          .  O.schema
          .  O.description
          ?~ "Request to receive authentication token, here we expect code that "
          <> "was sended via separate endpoint for such requests"
        ]

-- | Sign In response.
newtype SignInResp = SignInResp
  { si_respToken :: JwtToken
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON (JsonCamelCase "si_resp") SignInResp

instance ToSchema SignInResp where
  declareNamedSchema = genericNamedSchema @(JsonCamelCase "si_resp") extra
    where
      extra =
        [ mapped
            .  O.schema
            .  O.description
            ?~ "Returns JWT token for further use as authenticated user."
        ]

