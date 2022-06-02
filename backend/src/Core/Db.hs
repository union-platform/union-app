-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Tools to work with DB.
module Core.Db
  ( -- * Database pool
    DbCredentials(..)
  , DbPool
  , WithDb
  , withDb
  , withPool

    -- * Accessing DB
  , Id(..)
  , runTransaction
  , runStatement

    -- * Migrations
  , migrate
  , getAppliedMigrations
  , showMigration
  , pruneDb
  ) where

import Relude

import qualified Hasql.Pool as Pool

import Control.Exception.Safe (bracket)
import Data.Aeson (FromJSON, ToJSON)
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
import Hasql.Statement (Statement)
import Hasql.Transaction (Transaction, sql, statement)
import Hasql.Transaction.Sessions (IsolationLevel(..), Mode(..), transaction)
import Rel8 (DBEq, DBNum, DBType)

import Core.Error (WithError, liftError)
import Core.Has (Has, grab)
import Core.Logger (Severity(Error))


-- | Constraint for monadic actions that wants access to database.
type WithDb env m = (MonadReader env m, Has DbPool env, MonadIO m)

-- | Pool of PostgreSQL database connections.
type DbPool = Pool.Pool

-- | Represent PostgreSQL credentials.
-- Each parameter setting is in the form keyword = value.
-- Example: host=localhost port=5432 dbname=mydb connect_timeout=10
-- see [documentation](https://www.postgresql.org/docs/13/libpq-connect.html#LIBPQ-CONNSTRING)
newtype DbCredentials = DbCredentials { getDbCredentials :: Text}
  deriving stock Generic
  deriving newtype (Show, Eq, Semigroup, Monoid, ToJSON, FromJSON)

-- | Create 'Pool.Pool'.
createPool :: Int -> NominalDiffTime -> DbCredentials -> IO DbPool
createPool size timeout credentials =
  Pool.acquire (size, timeout, encodeUtf8 $ getDbCredentials credentials)
{-# INLINE createPool #-}

-- | Release 'Pool.Pool'.
destroyPool :: DbPool -> IO ()
destroyPool = Pool.release
{-# INLINE destroyPool #-}

-- | Helper to establish connection safely.
withDb :: Int -> NominalDiffTime -> DbCredentials -> (DbPool -> IO a) -> IO a
withDb size timeout credentials =
  bracket (createPool size timeout credentials) destroyPool

-- | Perform action that needs database connection.
withPool :: (WithDb env m, WithError UsageError m) => Session a -> m a
withPool action = do
  pool <- grab @DbPool
  withFrozenCallStack (liftIO (Pool.use pool action) >>= liftError Error)
{-# INLINE withPool #-}


-- | Type to represent id in database.
newtype Id a = Id { getId :: Int64 }
  deriving stock Generic
  deriving newtype (Show, Eq, DBType, DBEq, DBNum)

-- | Runs 'Transaction' with 'DbPool'.
runTransaction
  :: (WithDb env m, WithError UsageError m) => Transaction a -> m a
runTransaction =
  withFrozenCallStack (withPool . transaction Serializable Write)
{-# INLINE runTransaction #-}

-- | Runs 'Statement' as 'Transaction'.
runStatement :: (WithDb env m, WithError UsageError m) => Statement () a -> m a
runStatement = withFrozenCallStack (runTransaction . statement ())
{-# INLINE runStatement #-}


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
