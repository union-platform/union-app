-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module introduce aliases to use for @servant-generic@ types and
-- functions writing.
module Union.Server
  ( application
  ) where

import Relude

import Control.Monad.Except (liftEither)
import Servant.API.Generic ((:-), ToServantApi, toServant)
import Servant.Server (Application, Handler, Server, hoistServer, serve)

import Core.Monad (runAppLogIO)

import Union.Account.Server (AccountAPI, accountEndpoints)
import Union.App.Env (App, Env, Union)
import Union.App.Error (toHttpError)


-- | Union web application
application :: Env -> Application
application env = serve api $ server env

-- | Union web server.
server :: Env -> Server API
server env = hoistServer api toHandler (toServant endpoints)
  where
    toHandler :: App a -> Handler a
    toHandler app =
      liftIO (runAppLogIO env app) >>= liftEither . first toHttpError

-- | Represents combination of all endpoints, available in Union.
newtype Endpoints route = Endpoints
  { eAccount :: route :- AccountAPI
  }
  deriving stock Generic

-- | Combination of all endpoints, available in Union.
endpoints :: Endpoints Union
endpoints = Endpoints { eAccount = toServant accountEndpoints }

-- | Helper type to represent Union API in terms of Servant.
type API = ToServantApi Endpoints

-- | Helper function for referring to whole Union API.
api :: Proxy API
api = Proxy
