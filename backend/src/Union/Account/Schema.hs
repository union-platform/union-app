-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents account related database schema.
module Union.Account.Schema
  ( -- * Account
    Account(..)
  , accountSchema
  ) where

import Relude

import Data.Time.Clock (UTCTime)
import Rel8
  (Column, Name, Rel8able, Result, TableSchema(..))

import Core.Db (Id)
import Core.Sender (Phone)


-- | Simple account (without profile).
data Account f = Account
  { aAccountId   :: Column f (Id Account)
  , aPhone       :: Column f Phone
  , aCreatedAt   :: Column f UTCTime
  , aActivatedAt :: Column f (Maybe UTCTime)
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving stock instance f ~ Result => Show (Account f)

-- | Schema to represent 'Account' in Database.
accountSchema :: TableSchema (Account Name)
accountSchema = TableSchema
  { name    = "account"
  , schema  = Nothing
  , columns = Account
    { aAccountId   = "account_id"
    , aPhone       = "phone"
    , aCreatedAt   = "created_at"
    , aActivatedAt = "activated_at"
    }
  }
