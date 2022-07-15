-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents profile related service; here is actions with DB or
-- another external services.
module Union.Account.Profile.Service
  ( findProfile
  , createProfile
  , searchInterests
  , createInterest
  , updateUserInterests
  , getUserInterests
  ) where

import Relude

import qualified Rel8 as Sql

import Hasql.Statement (Statement)
import Rel8 ((&&.), (==.), Expr, Result)

import Core.Error (throwError)
import Core.Logger (Severity(..))

import Data.List ((\\))
import Hasql.Transaction (statement)
import Union.Account.Profile.Schema
  ( Interest(..)
  , InterestId
  , InterestMap(..)
  , Profile(..)
  , interestMapSchema
  , interestSchema
  , profileSchema
  )
import Union.Account.Schema (AccountId)
import Union.Account.Types (UserName)
import Union.App.Db (executeS, executeT, mkIlike, nextId, selectOne)
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

-- | Finds 'Interest's in DB by given text.
searchInterests :: (WithDb m, WithError m) => Maybe Text -> m [Interest Result]
searchInterests name = executeS . Sql.select . Sql.limit 10 $ do
  interests@Interest { iName } <- Sql.each interestSchema
  whenJust name $ \n -> Sql.where_ $ mkIlike n `Sql.ilike` iName
  pure interests

-- | Creates 'Interest' in DB by given name.
createInterest :: (WithDb m, WithError m) => Text -> m (Interest Result)
createInterest name = do
  interest <- executeS insertInterest
  maybe (throwError Error $ DbError Nothing) pure $ listToMaybe interest
  where
    insertInterest :: Statement () [Interest Result]
    insertInterest = Sql.insert $ Sql.Insert
      { into       = interestSchema
      , rows       = Sql.values
        [ Interest
            { iInterestId = nextId "interest_interest_id" :: Expr InterestId
            , iName       = Sql.lit name
            }
        ]
      , onConflict = Sql.Abort
      , returning  = Sql.Projection id
      }

-- | Gets user's 'Interest'.
getUserInterests :: (WithDb m, WithError m) => AccountId -> m [Interest Result]
getUserInterests = executeS . getUserInterestsQ
{-# INLINE getUserInterests #-}

-- | Gets user's 'Interest' query.
getUserInterestsQ :: AccountId -> Statement () [Interest Result]
getUserInterestsQ aId =
  Sql.select $ Sql.each interestMapSchema >>= getInterests
  where
    getInterests InterestMap {..} = do
      interests@Interest { iInterestId } <- Sql.each interestSchema
      Sql.where_ $ imInterestId ==. iInterestId &&. imAccountId ==. Sql.lit aId
      pure interests

-- | Adds 'Interest' to 'InterestMap' by given 'AccountId' and list of 'Interest'.
updateUserInterests
  :: (WithDb m, WithError m) => AccountId -> [InterestId] -> m ()
updateUserInterests aId interests = executeT $ do
  userInterests <- statement () $ map iInterestId <$> getUserInterestsQ aId
  -- delete interests that are not in request
  mapM_ (statement () . deleteInterestMaps) $ userInterests \\ interests
  -- add interests that are not in current
  statement () . insertInterestMaps $ interests \\ userInterests
  where
    deleteInterestMaps :: InterestId -> Statement () ()
    deleteInterestMaps iId = Sql.delete $ Sql.Delete
      { from        = interestMapSchema
      , using       = pass
      , deleteWhere = \_ InterestMap {..} ->
        imAccountId ==. Sql.lit aId &&. imInterestId ==. Sql.lit iId
      , returning   = pass
      }

    insertInterestMaps :: [InterestId] -> Statement () ()
    insertInterestMaps iIds = Sql.insert $ Sql.Insert
      { into       = interestMapSchema
      , rows       = Sql.values $ map
        (\iId -> InterestMap
          { imInterestId = Sql.lit iId
          , imAccountId  = Sql.lit aId
          }
        )
        iIds
      , onConflict = Sql.DoNothing
      , returning  = pass
      }
