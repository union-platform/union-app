-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents account related database schema.
module Union.Account.Profile.Schema
  ( -- * Profile
    Profile(..)
  , profileSchema

    -- * Interest
  , Interest (..)
  , InterestId
  , interestSchema
  , InterestMap (..)
  , interestMapSchema
  ) where

import Relude

import qualified Data.OpenApi as O

import Rel8 (Column, Name, Rel8able, Result, TableSchema(..))
import Data.Aeson (ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON)
import Deriving.Aeson (AesonOptions (..))
import Data.OpenApi (ToSchema (..))
import Control.Lens ((?~), mapped)

import Core.Db (Id (..))
import Core.Json (JsonCamelCase)
import Core.Swagger (genericNamedSchema)

import Union.Account.Types (UserName)
import Union.Account.Schema (AccountId)


-- | User's profile.
data Profile f = Profile
  { pAccountId :: Column f AccountId
  , pName      :: Column f UserName
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving stock instance f ~ Result => Show (Profile f)

-- | Schema to represent 'Profile' in Database.
profileSchema :: TableSchema (Profile Name)
profileSchema = TableSchema
  { name    = "profile"
  , schema  = Nothing
  , columns = Profile { pAccountId = "account_id", pName = "name" }
  }


-- | Interest identifier.
type InterestId = Id Interest

-- | Interests.
data Interest f = Interest
  { iInterestId :: Column f InterestId
  , iName       :: Column f Text
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving stock instance f ~ Result => Show (Interest f)
deriving stock instance f ~ Result => Eq   (Interest f)

instance f ~ Result => FromJSON (Interest f) where
  parseJSON = genericParseJSON $ aesonOptions @(JsonCamelCase "i")

instance f ~ Result => ToJSON (Interest f) where
  toJSON = genericToJSON $ aesonOptions @(JsonCamelCase "i")

instance f ~ Result => ToSchema (Interest f) where
  declareNamedSchema = genericNamedSchema @(JsonCamelCase "i") extra
    where
      extra =
        [ mapped . O.schema . O.example ?~ toJSON
          (Interest (Id 1) "Lev Tolstoy")
        , mapped . O.schema . O.description ?~ "Interest"
        ]

-- | Schema to represent 'Interest' in Database.
interestSchema :: TableSchema (Interest Name)
interestSchema = TableSchema
  { name    = "interest"
  , schema  = Nothing
  , columns = Interest { iInterestId = "interest_id", iName = "name" }
  }

-- | Map between 'Interest' and 'Account'.
data InterestMap f = InterestMap
  { imInterestId :: Column f InterestId
  , imAccountId  :: Column f AccountId
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving stock instance f ~ Result => Show (InterestMap f)

-- | Schema to represent 'InterestMap' in Database.
interestMapSchema :: TableSchema (InterestMap Name)
interestMapSchema = TableSchema
  { name    = "interest_map"
  , schema  = Nothing
  , columns = InterestMap
    { imInterestId = "interest_id"
    , imAccountId  = "account_id"
    }
  }
