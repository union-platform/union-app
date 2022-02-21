-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: MPL-2.0

-- | This module introduce aliases to use for @servant-generic@ types and
-- functions writing.
module Core.Servant
  ( AppServer
  , ToApi
  , RequiredHeader
  ) where

import Relude

import Servant.API (Header', Required, Strict)
import Servant.API.Generic (ToServantApi)
import Servant.Server.Generic (AsServerT)


-- | A type alias for specifying the app server.
type AppServer app = AsServerT app

-- | Simplified alias for 'ToServantApi'.
type ToApi (site :: Type -> Type) = ToServantApi site

-- | Non-optional header for strictly parsed text.
type RequiredHeader = Header' '[Required , Strict]
