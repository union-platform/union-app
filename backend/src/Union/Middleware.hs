-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module introduce middleware for Union server.s
module Union.Middleware
  ( applyMiddleware
  ) where

import Relude

import Network.Wai (Application, Middleware)


-- | Helper to apply list of 'Middleware' to 'Application'.
-- This list will be combined in order as one middleware; the order of
-- execution is from top to bottom (from left to right).
applyMiddleware :: [Middleware] -> Application -> Application
applyMiddleware = foldl' (.) id
