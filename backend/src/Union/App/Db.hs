-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Tools to work with DB.
module Union.App.Db
  ( executeS
  , executeT
  , selectOne
  , selectExists
  , nextId
  , initDb
  , runDb
  ) where

import Relude

import qualified Rel8 as Sql
import qualified Rel8.Expr.Num as Sql

import Control.Monad.Except (MonadError)
import Hasql.Pool (UsageError)
import Hasql.Statement (Statement)
import Hasql.Transaction (Transaction)
import Rel8 (DBNum, Expr, Query, Serializable, exists, limit, select)

import Core.Db (migrate, runStatement, runTransaction)
import Core.Error (ErrorWithSource, catchError, showErr, toAppError)
import Core.Logging (logInfo)

import Union.App.Configuration (DatabaseConfig(..))
import Union.App.Env (WithDb, WithError, WithLog, kill)
import Union.App.Error (Error(DbError))


-- | Executes 'Statement'.
executeS :: (WithDb m, WithError m) => Statement () a -> m a
executeS = runDb . runStatement
{-# INLINE executeS #-}

-- | Executes 'Transaction'.
executeT :: (WithDb m, WithError m) => Transaction a -> m a
executeT = runDb . runTransaction
{-# INLINE executeT #-}

-- | Helper to select one record.
selectOne
  :: forall exprs a
   . (Serializable exprs a)
  => Query exprs
  -> Statement () (Maybe a)
selectOne = fmap listToMaybe . select . limit 1
{-# INLINE selectOne #-}

-- | Helper to check existence.
selectExists :: Query exprs -> Statement () Bool
selectExists = fmap (all (== True)) . select . exists
{-# INLINE selectExists #-}

-- | Helper to generate next serial id by given field name.
nextId :: DBNum a => String -> Expr a
nextId name = Sql.fromIntegral . Sql.nextval $ name <> "_seq"
{-# INLINE nextId #-}

-- | Init DB with migrations.
initDb
  :: (MonadFail m, WithDb m, WithLog m, WithError m) => DatabaseConfig -> m ()
initDb DatabaseConfig {..} = do
  logInfo "Running migrations..."
  result <- runDb (migrate dcMigrations) `catchError` (kill . showErr)
  whenJust result $ \e -> kill ("Cannot apply migrations: " <> showErr e)
  logInfo "Migrations have been applied"

-- | Helper to run action with DB connection and conversion internal error
-- to 'DbError'.
runDb
  :: MonadError (ErrorWithSource Error) m
  => ExceptT (ErrorWithSource UsageError) m a
  -> m a
runDb action = withFrozenCallStack (toAppError (DbError . Just) action)
{-# INLINE runDb #-}
