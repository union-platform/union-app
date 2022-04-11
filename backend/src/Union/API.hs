-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Union.API
  ( API
  ) where

import Servant (type (:>), Get, JSON)

import Union.App.Configuration (Config)


-- This is main API type (currently, it's sample only).
type API = "config" :> Get '[JSON] Config
