-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents profile related service; here is actions with DB or
-- another external services.
module Union.Account.Profile.Service
  ( findProfile
  , createProfile
  ) where

import Relude

import qualified Rel8 as Sql

import Hasql.Statement (Statement)
import Rel8 ((==.), Result)

import Core.Error (throwError)
import Core.Logger (Severity(..))

import Union.Account.Profile.Schema (Profile(..), profileSchema)
import Union.Account.Schema (AccountId)
import Union.Account.Types (UserName)
import Union.App.Db (executeS, selectOne)
import Union.App.Env (WithDb, WithError)
import Union.App.Error (Error(..))


-- | Finds 'Profile'' by given 'AccountId'.
findProfile
  :: (WithDb m, WithError m) => AccountId -> m (Maybe (Profile Result))
findProfile aId = executeS . selectOne $ do
  profile@Profile { pAccountId } <- Sql.each profileSchema
  Sql.where_ $ pAccountId ==. Sql.lit aId
  pure profile

-- | Creates 'Profile' by given 'AccountId' and 'UserName'.
createProfile
  :: (WithDb m, WithError m) => AccountId -> UserName -> m (Profile Result)
createProfile aId name = do
  profile <- executeS insertProfile
  maybe (throwError Error $ DbError Nothing) pure $ listToMaybe profile
  where
    insertProfile :: Statement () [Profile Result]
    insertProfile = Sql.insert $ Sql.Insert
      { into       = profileSchema
      , rows       = Sql.values
        [Profile { pAccountId = Sql.lit aId, pName = Sql.lit name }]
      , onConflict = Sql.Abort
      , returning  = Sql.Projection id
      }
