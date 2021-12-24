-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Union.API
  ( API
  , api
  ) where

import Relude

import Servant (Get, JSON, type (:>))

import Union.Configuration (UnionConfig)


-- This is main API type (currently, it's sample only).
type API = "config" :> Get '[JSON] UnionConfig

-- 'Proxy' to 'API'.
api :: Proxy API
api = Proxy
