-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: MPL-2.0

-- | Tools to work with DB.
module Core.Db
  ( -- * Database pool
    DbPool
  , WithDb
  , withDb
  , withPool
  ) where

import Relude

import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Sql

import Control.Exception.Safe (bracket)
import Data.Time.Clock (NominalDiffTime)

import Core.Error (WithError, liftError)
import Core.Has (Has, grab)


-- | Constraint for monadic actions that wants access to database.
type WithDb env m = (MonadReader env m, Has DbPool env, MonadIO m)

-- | Pool of PostgreSQL database connections.
type DbPool = Pool.Pool

-- | Create 'Pool.Pool'.
createPool :: Int -> NominalDiffTime -> Text -> IO DbPool
createPool size timeout credentials =
  Pool.acquire (size, timeout, encodeUtf8 credentials)

-- | Release 'Pool.Pool'.
destroyPool :: DbPool -> IO ()
destroyPool = Pool.release

-- | Helper to establish connection safely.
withDb :: Int -> NominalDiffTime -> Text -> (DbPool -> IO a) -> IO a
withDb size timeout credentials =
  bracket (createPool size timeout credentials) destroyPool

-- | Perform action that needs database connection.
withPool :: (WithDb env m, WithError Pool.UsageError m) => Sql.Session a -> m a
withPool action = do
  pool <- grab @DbPool
  liftIO (Pool.use pool action) >>= liftError
{-# INLINE withPool #-}
