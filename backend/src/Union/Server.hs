-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module introduce aliases to use for @servant-generic@ types and
-- functions writing.
module Union.Server
  ( application
  ) where

import Relude

import qualified Data.OpenApi as O
import qualified Servant.OpenApi as O

import Control.Lens ((.~), (?~))
import Control.Monad.Except (liftEither)
import Data.Version (showVersion)
import Servant.API (type (:<|>)((:<|>)))
import Servant.API.Generic ((:-), ToServantApi, toServant)
import Servant.OpenApi (HasOpenApi(..))
import Servant.Server (Application, Handler, Server, hoistServer, serve)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

import qualified Paths_union as Meta

import Core.Monad (runAppLogIO)
import Core.Swagger (WithSwagger)

import Union.Account.Server (AccountAPI, accountEndpoints)
import Union.App.Env (App, Env, Union)
import Union.App.Error (toHttpError)


-- | Union web application
application :: Env -> Application
application env =
  serve (Proxy @(WithSwagger "docs" API)) $ server env :<|> swagger

-- | Union web server.
server :: Env -> Server API
server env = hoistServer api toHandler $ toServant endpoints
  where
    toHandler :: App a -> Handler a
    toHandler app =
      liftIO (runAppLogIO env app) >>= liftEither . first toHttpError

-- | Represents combination of all endpoints, available in Union.
newtype Endpoints route = Endpoints
  { eAccount :: route :- AccountAPI
  }
  deriving stock Generic

-- | Represents combination of all endpoints, available in Union.
endpoints :: Endpoints Union
endpoints = Endpoints { eAccount = toServant accountEndpoints }

-- | Helper type to represent Union API in terms of Servant.
type API = ToServantApi Endpoints

-- | Helper function for referring to whole Union API.
api :: Proxy API
api = Proxy

-- | Generates swagger documentation for 'Endpoints'.
swagger :: Server (SwaggerSchemaUI schema dir)
swagger =
  swaggerSchemaUIServer
    $  toOpenApi api
    &  O.info
    .~ ( mempty
       & (O.title .~ "Union API")
       & (O.version .~ toText (showVersion Meta.version))
       & (  O.license
         ?~ (  "AGPL-3.0-or-later"
            &  O.url
            ?~ O.URL
                 "https://github.com/union-platform/union-app/blob/master/LICENSE"
            )
         )
       & (  O.contact
         ?~ ( mempty
            & (O.name ?~ "Union Platform")
            & (O.url ?~ O.URL "https://unionapp.cc")
            )
         )
       )
    &  O.applyTagsFor
         (O.subOperations (Proxy :: Proxy AccountAPI) api)
         ["Account"]
