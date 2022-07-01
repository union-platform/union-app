-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents signIn related database schema.
module Union.Account.SignIn.Schema
  ( -- * Confirmation
    ConfirmationScope(..)
  , Confirmation(..)
  , confirmationSchema

    -- * Auth Log
  , AuthLog(..)
  , authLogSchema
  ) where

import Relude

import qualified Rel8 as Sql

import Data.Time.Clock (UTCTime)
import Rel8
  (Column, DBEnum(..), DBEq, DBType, Name, Rel8able, Result, TableSchema(..))

import Core.Db (Id)
import Core.Sender (ConfirmationCode)

import Union.Account.Schema (Account)
import Union.Account.SignIn.Types (UserAgent)


-- | Scope of 'Confirmation' code.
data ConfirmationScope = SignIn
  deriving stock (Generic, Read, Show, Eq, Enum)
  deriving (DBType, DBEq) via Sql.Enum ConfirmationScope

instance DBEnum ConfirmationScope where
  enumTypeName = "c_scope"

-- | Confirmation code.
data Confirmation f = Confirmation
  { cAccountId :: Column f (Id Account)
  , cScope     :: Column f ConfirmationScope
  , cCode      :: Column f ConfirmationCode
  , cCreatedAt :: Column f UTCTime
  , cExpiredAt :: Column f UTCTime
  , cUsed      :: Column f Bool
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving stock instance f ~ Result => Show (Confirmation f)

-- | Schema to represent 'Confirmation' in Database.
confirmationSchema :: TableSchema (Confirmation Name)
confirmationSchema = TableSchema
  { name    = "confirmation"
  , schema  = Nothing
  , columns = Confirmation
    { cAccountId = "account_id"
    , cScope     = "scope"
    , cCode      = "code"
    , cCreatedAt = "created_at"
    , cExpiredAt = "expired_at"
    , cUsed      = "used"
    }
  }


-- | Auth log.
data AuthLog f = AuthLog
  { alAccountId :: Column f (Id Account)
  , alCode      :: Column f ConfirmationCode
  , alIp        :: Column f Text
  , alClient    :: Column f (Maybe UserAgent)
  , alCreatedAt :: Column f UTCTime
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving stock instance f ~ Result => Show (AuthLog f)

-- | Schema to represent 'AuthLog' in Database.
authLogSchema :: TableSchema (AuthLog Name)
authLogSchema = TableSchema
  { name    = "auth_log"
  , schema  = Nothing
  , columns = AuthLog
    { alAccountId = "account_id"
    , alCode      = "code"
    , alIp        = "ip"
    , alClient    = "client"
    , alCreatedAt = "created_at"
    }
  }
