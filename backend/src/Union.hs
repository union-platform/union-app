-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | The main module that reexports all main functionality.
module Union
  ( union
  ) where

import Relude

import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)

import qualified Core

import Core.Json (packJson)
import Core.Logging (logInfo)

import Union.API (API)
import Union.App.Configuration (Config(..))
import Union.App.Db (initDb)
import Union.App.Env (runWithEnv)


-- | Union entrypoint.
union :: Maybe FilePath -> IO ()
union configPath = runWithEnv configPath $ do
  config@Config {..} <- Core.grab @Config
  logInfo $ "Starting application with configuration: \n" <> packJson config
  initDb cDatabase
  liftIO $ run cAppPort . serve (Proxy @API) $ pure config
