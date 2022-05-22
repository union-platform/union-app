-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module introduce middleware for Union server.s
module Union.Middleware
  ( applyMiddleware

    -- * Middleware list
  , loggingMiddleware
  ) where

import Relude

import Network.Wai (Application, Middleware)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import Core.Logger (Severity(..))


-- | Helper to apply list of 'Middleware' to 'Application'.
-- This list will be combined in order as one middleware; the order of
-- execution is from top to bottom (from left to right).
applyMiddleware :: [Middleware] -> Application -> Application
applyMiddleware = foldl' (.) id

-- | This function creates a logging middleware - verbose is severity is 'Debug',
-- non-verbose - if 'Info', and disabled otherwise.
loggingMiddleware :: Severity -> Middleware
loggingMiddleware = \case
  Debug -> logStdoutDev
  Info  -> logStdout
  _     -> id
