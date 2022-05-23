-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides logging ability. Currently it only reexports @co-log@.
module Core.Logger
  ( Log
  , WithLog
  , Logger
  , LoggerT
  , setLogger
  , emptyLogger

    -- * Reexport
  , module Colog
  ) where

import Relude

import qualified Colog (LoggerT, Message, WithLog)

import Colog
  hiding (LoggerT, Message, WithLog, logTextStdout, richMessageAction)
import Data.Aeson (FromJSON(..), ToJSON(..), genericToJSON, withText)

import Core.Json (jsonSumTypeOptions)


-- | Type alias to not confuse that 'Colog.Message.Message' is about logging.
type Log = Colog.Message

-- | Constrain for actions with logging.
type WithLog env m = Colog.WithLog env Log m

-- | Alias for logger, needs to simplify signatures.
type Logger m = LogAction m Log

-- | Alias for logger transformer.
type LoggerT m = Colog.LoggerT Log m

-- | Helper to set logging level.
--  * 'Debug' - Information useful for debug purposes
--  * 'Info' - Normal operational information
--  * 'Warning' - General warnings, non-critical failures
--  * 'Error' - General errors/severe errors
setLogger :: MonadIO m => Severity -> Logger m
setLogger severity = filterBySeverity severity msgSeverity richMessageAction
{-# INLINE setLogger #-}

-- | Logger which do nothing.
emptyLogger :: Applicative m => Logger m
emptyLogger = LogAction $ const pass
{-# INLINE emptyLogger #-}

-- | Action that constructs 'RichMessage' and prints formatted 'Log' for it
-- to 'stdout'.
richMessageAction :: MonadIO m => Logger m
richMessageAction = upgradeMessageAction defaultFieldMap
  $ cmapM fmtRichMessageDefault logTextStdout
{-# INLINE richMessageAction #-}
{-# SPECIALIZE richMessageAction :: Logger IO #-}

-- | Action to log 'Text' to stdout.
-- Note: we append newline before printing to fix concurrent issues.
logTextStdout :: MonadIO m => LogAction m Text
logTextStdout = LogAction $ \m -> putText $ m <> "\n"
{-# INLINE logTextStdout #-}
{-# SPECIALIZE logTextStdout :: LogAction IO Text #-}

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
