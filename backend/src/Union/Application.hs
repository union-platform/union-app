-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Union.Application
  ( Union
  , UnionServer

  , WithDb
  , WithError

  , Env
  , ErrorType (..)

  , runUnion
  , union
  ) where

import Relude

import Network.Wai.Handler.Warp (run)
import Servant (serve)

import qualified Core

import Core.Db (DbPool)
import Core.Has (Has, grab)
import Union.API (api)
import Union.Configuration
  (DatabaseConfig(..), UnionConfig(..), UnionOptions(..), defaultUnionConfig,
  loadConfig)


-- | Main application monad.
type Union = Core.App ErrorType Env

-- | Type alias to specify application server.
type UnionServer = Core.AppServer Union

-- | Constrain to actions with DB access.
type WithDb m = Core.WithDb Env m

-- | Constrain to actions that can throw and catch pure errors with stack position.
type WithError m = Core.WithError ErrorType m

-- | Union environment.
data Env = Env
  { eConfig :: !UnionConfig
  , eDbPool :: !DbPool
  } deriving (Has UnionConfig) via Core.Field "eConfig" Env
    deriving (Has DbPool)      via Core.Field "eDbPool" Env

-- | Errors in Union.
data ErrorType
  = NotFound
  -- ^ General not found.
  | ServerError Text
  -- ^ Some exceptional circumstance has happened to stop execution and return.
  -- Optional text to provide some context in server logs.
  | NotAllowed Text
  -- ^ A required permission level was not met. Optional text to provide some context.
  | Invalid Text
  -- ^ Given inputs do not conform to the expected format or shape. Optional
  -- text to provide some context in server logs.
  | MissingHeader Text
  -- ^ Some header expected, but not present in header list.
  | HeaderDecodeError Text
  -- ^ An authentication header that was required was provided but not in a
  -- format that the server can understand.
  | DbError Text
  -- ^ Data base specific errors.
  deriving stock (Show, Eq)

-- | Runs the web server which serves Union API.
union :: Union ()
union = do
  config <- grab @UnionConfig
  liftIO $ run (ucAppPort config) . serve api $ pure config

-- | Create 'Env' and run 'Union' action with this environment.
runUnion :: UnionOptions -> Union a -> IO a
runUnion options action = do
  config <- maybe (pure defaultUnionConfig) loadConfig $ uoConfig options
  let DatabaseConfig{..} = ucDatabase config
  Core.withDb dcPoolSize dcTimeout dcCredentials $ \pool -> do
    let env = Env
          { eConfig = config
          , eDbPool = pool
          }
    liftIO $ Core.runApp env action
