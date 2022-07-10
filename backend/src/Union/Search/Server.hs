-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents search related endpoints and its handlers.
module Union.Search.Server
  ( SearchAPI
  , SearchEndpoints(..)
  , searchEndpoints
  ) where

import Relude

import Servant ((:>))
import Servant.API (Description, Get, JSON, NamedRoutes, QueryParam, Summary)
import Servant.API.Generic ((:-))

import Union.Account.Schema (AccountId)
import Union.App.Env (Union)
import Union.Search.Types (SuggestInterestsResp(..))


-- | Helper type to represent Search API in terms of Servant.
type SearchAPI = "search" :> NamedRoutes SearchEndpoints

-- | Represents API related to search.
newtype SearchEndpoints mode = SearchEndpoints
  { _suggestInterests :: mode
      :- "interests"
      :> Summary "Get interests suggestion"
      :> Description
        "Helper endpoint to suggest interests for user while he is typing in \
        \form, returns list of matched interests."
      :> QueryParam "q" Text
      :> Get '[JSON] SuggestInterestsResp
  }
  deriving stock Generic

-- | Endpoints related to search.
searchEndpoints :: AccountId -> SearchEndpoints Union
searchEndpoints _aId =
  SearchEndpoints { _suggestInterests = suggestInterestsHandler }


-- | Handler to suggest 'Interest'.
suggestInterestsHandler :: Monad m => Maybe Text -> m SuggestInterestsResp
suggestInterestsHandler _q = do
  pure $ SuggestInterestsResp []
