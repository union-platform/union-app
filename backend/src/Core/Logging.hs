-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides logging ability. Currently it only reexports @co-log@.
module Core.Logging
  ( Log
  , setLogger

    -- * Reexport
  , module Colog
  ) where

import Relude

import qualified Colog.Message (Message)

import Colog hiding (Message)
import Data.Aeson (FromJSON(..), ToJSON(..), genericToJSON, withText)

import Core.Json (jsonSumTypeOptions)


-- | Type alias to not confuse that 'Colog.Message.Message' is about logging.
type Log = Colog.Message.Message

-- | Helper to set logging level.
--  * 'Debug' - Information useful for debug purposes
--  * 'Info' - Normal operational information
--  * 'Warning' - General warnings, non-critical failures
--  * 'Error' - General errors/severe errors
setLogger :: MonadIO m => Severity -> LogAction m Log
setLogger severity = filterBySeverity severity msgSeverity richMessageAction


deriving stock instance Generic Severity

instance FromJSON Severity where
  parseJSON = withText "LoggingConfig" $ \case
    "debug"   -> pure Debug
    "info"    -> pure Info
    "warning" -> pure Warning
    "error"   -> pure Error
    v         -> fail $ "Invalid severity: " <> show v

instance ToJSON Severity where
  toJSON = genericToJSON jsonSumTypeOptions
