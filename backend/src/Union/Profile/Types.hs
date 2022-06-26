-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents profile related types, which is not related to DB,
-- mostly here is helper types for API.
module Union.Profile.Types
  ( UserName(..)
  ) where

import Relude

import Data.Aeson (FromJSON, ToJSON(..))
import Data.OpenApi (ToParamSchema, ToSchema(..))
import Rel8 (DBEq, DBType)
import Servant.API (FromHttpApiData, ToHttpApiData)


-- | Represents user name.
newtype UserName = UserName
  { getUserName :: Text
  }
  deriving stock Generic
  deriving newtype
    ( Show, Eq, DBType, DBEq, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData
    , ToParamSchema, ToSchema
    )
