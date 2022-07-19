-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents search related types, which is not related to DB,
-- mostly here is helper types for API.
module Union.Search.Types
  ( SuggestInterestsResp(..)
  ) where

import Relude

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.OpenApi (ToSchema)
import Rel8 (Result)

import Union.Account.Profile.Schema (Interest)


-- | 'Interest' suggestion response.
newtype SuggestInterestsResp = SuggestInterestsResp
  { si_respInterests :: [Interest Result]
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema)
