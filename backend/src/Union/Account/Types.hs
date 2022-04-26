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

import Data.Aeson (FromJSON, ToJSON(..))
import Deriving.Aeson (CustomJSON(..))

import Core.Json (JsonCamelCase)
import Core.Jwt (JwtToken)
import Core.Sender (ConfirmationCode(..), Phone(..))


-- | Sign In request.
newtype RequestCodeReq = RequestCodeReq
  { rc_reqPhone :: Phone
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON (JsonCamelCase "rc_req") RequestCodeReq

-- | Sign In request.
data SignInReq = SignInReq
  { si_reqPhone :: Phone
  , si_reqCode  :: ConfirmationCode
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON (JsonCamelCase "si_req") SignInReq


-- | Sign In response.
newtype SignInResp = SignInResp
  { si_respToken :: JwtToken
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON (JsonCamelCase "si_resp") SignInResp
