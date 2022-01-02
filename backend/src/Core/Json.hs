-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module provides tools to work with JSON.
module Core.Json
  ( jsonCamelOptions
  ) where

import Relude

import qualified Data.Aeson as JSON

import Data.Aeson (Options(fieldLabelModifier, omitNothingFields))
import Data.Aeson.Casing (aesonPrefix, camelCase)


-- | Options for JSON types using train case.
jsonCamelOptions :: Options
jsonCamelOptions = JSON.defaultOptions
  { omitNothingFields = True
  , fieldLabelModifier = fieldLabelModifier $ aesonPrefix camelCase
  }
