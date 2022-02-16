-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Tools to work with DB.
module Core.Db
  ( -- * Database pool
    DbPool
  , WithDb
  , withDb
  , withPool

    -- * Working with DB
  , runTransaction
  , migrate
  , getAppliedMigrations
  , showMigration
  , pruneDb
  ) where

import Relude

import qualified Hasql.Pool as Pool

import Control.Exception.Safe (bracket)
import Data.Time.Clock (NominalDiffTime)
import Hasql.Migration
  ( MigrationCommand(MigrationInitialization)
  , MigrationError
  , SchemaMigration(..)
  , getMigrations
  , loadMigrationsFromDirectory
  , runMigration
  )
import Hasql.Pool (UsageError)
import Hasql.Session (Session)
import Hasql.Transaction (Transaction, sql)
import Hasql.Transaction.Sessions
  (IsolationLevel(Serializable), Mode(Write), transaction)

import Core.Error (WithError, liftError)
import Core.Has (Has, grab)
import Core.Logging (logIO)


-- | Constraint for monadic actions that wants access to database.
type WithDb env m = (MonadReader env m, Has DbPool env, MonadIO m)

-- | Pool of PostgreSQL database connections.
type DbPool = Pool.Pool

-- | Create 'Pool.Pool'.
createPool :: HasCallStack => Int -> NominalDiffTime -> Text -> IO DbPool
createPool size timeout credentials = logIO "Creating DB pool..."
  >> Pool.acquire (size, timeout, encodeUtf8 credentials)
{-# INLINE createPool #-}

-- | Release 'Pool.Pool'.
destroyPool :: HasCallStack => DbPool -> IO ()
destroyPool p = logIO "Destroying DB pool..." >> Pool.release p
{-# INLINE destroyPool #-}

-- | Helper to establish connection safely.
withDb :: Int -> NominalDiffTime -> Text -> (DbPool -> IO a) -> IO a
withDb size timeout credentials =
  bracket (createPool size timeout credentials) destroyPool

-- | Perform action that needs database connection.
withPool :: (WithDb env m, WithError UsageError m) => Session a -> m a
withPool action = do
  pool <- grab @DbPool
  liftIO (Pool.use pool action) >>= liftError
{-# INLINE withPool #-}


-- | Runs 'Transaction' with 'DbPool'.
runTransaction
  :: (WithDb env m, WithError UsageError m) => Transaction b -> m b
runTransaction = withPool . transaction Serializable Write
{-# INLINE runTransaction #-}

-- | Launch all migrations from provided dir.
migrate
  :: (WithDb env m, WithError UsageError m)
  => FilePath
  -> m (Maybe MigrationError)
migrate dir = do
  migrations <- liftIO $ loadMigrationsFromDirectory dir
  runTransaction . runMigrations $ MigrationInitialization : migrations

-- | Runs migrations, stop executing in case of error.
runMigrations :: [MigrationCommand] -> Transaction (Maybe MigrationError)
runMigrations []       = pure Nothing
runMigrations (m : ms) = runMigration m >>= \case
  Nothing -> runMigrations ms
  err     -> pure err

-- | Returns list of applied migrations.
getAppliedMigrations
  :: (WithDb env m, WithError UsageError m) => m [SchemaMigration]
getAppliedMigrations = runTransaction getMigrations

-- | Returns textual representation of 'SchemaMigration'.
showMigration :: SchemaMigration -> Text
showMigration SchemaMigration {..} =
  " - "
    <> decodeUtf8 schemaMigrationName
    <> " at "
    <> show schemaMigrationExecutedAt

-- | Recreates public schema.
pruneDb :: Transaction ()
pruneDb = sql "drop schema public cascade; create schema public;"
