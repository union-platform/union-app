-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents account related service; here is actions with DB or
-- another external services.
module Union.Account.Service
  ( findAccount
  , createAccount
  ) where

import Relude

import qualified Rel8 as Sql

import Hasql.Statement (Statement)
import Rel8 ((==.), Expr, Result)
import Rel8.Expr.Time (now)

import Core.Error (throwError)
import Core.Logger (Severity(..))
import Core.Sender (Phone)

import Union.Account.Schema (Account(..), AccountId, accountSchema)
import Union.App.Db (executeS, nextId, selectOne)
import Union.App.Env (WithDb, WithError)
import Union.App.Error (Error(..))


-- | Finds account by given 'Phone'.
findAccount :: (WithDb m, WithError m) => Phone -> m (Maybe (Account Result))
findAccount phone = executeS . selectOne $ do
  account@Account { aPhone } <- Sql.each accountSchema
  Sql.where_ $ aPhone ==. Sql.lit phone
  pure account

-- | Creates account by given 'Phone'.
createAccount :: (WithDb m, WithError m) => Phone -> m (Account Result)
createAccount phone = do
  account <- executeS insertAccount
  maybe (throwError Error $ DbError Nothing) pure $ listToMaybe account
  where
    insertAccount :: Statement () [Account Result]
    insertAccount = Sql.insert $ Sql.Insert
      { into       = accountSchema
      , rows       = Sql.values
        [ Account
            { aAccountId   = nextId "account_account_id" :: Expr AccountId
            , aPhone       = Sql.lit phone
            , aCreatedAt   = now
            , aActivatedAt = Sql.null
            }
        ]
      , onConflict = Sql.Abort
      , returning  = Sql.Projection id
      }
