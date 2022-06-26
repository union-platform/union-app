-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents account related database schema.
module Union.Profile.Schema
  ( -- * Profile
    Profile(..)
  , profileSchema
  ) where

import Relude

import Rel8 (Column, Name, Rel8able, Result, TableSchema(..))

import Core.Db (Id)

import Union.Account.Schema (Account)
import Union.Profile.Types (UserName)


-- | User's profile.
data Profile f = Profile
  { pAccountId :: Column f (Id Account)
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
