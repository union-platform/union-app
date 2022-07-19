-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents account related types, which is not related to DB,
-- mostly here is helper types for API.
module Union.Account.Types
  ( UserName
  , mkUserName
  ) where

import Relude

import qualified Data.Text as T

import Data.Aeson (FromJSON, ToJSON(..))
import Data.OpenApi (ToSchema)
import Rel8 (DBEq, DBType)
import Servant.API (FromHttpApiData, ToHttpApiData)
import Text.Regex (matchRegex, mkRegex)

-- | Represents user name.
newtype UserName = UserName
  { getUserName :: Text
  }
  deriving stock Generic
  deriving newtype
    ( Show, Eq, DBType, DBEq, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData
    , ToSchema
    )

-- | Creates 'UserName' if provided input is valid.
mkUserName :: Text -> Maybe UserName
mkUserName (T.strip -> name) = case matchRegex r $ toString name of
  Nothing -> Nothing
  Just _  -> Just $ UserName name
  where r = mkRegex "^[a-zA-Z ]{4,32}$"
