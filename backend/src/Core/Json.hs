-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module provides tools to work with JSON.
module Core.Json
  ( packJson
  , jsonCamelOptions
  , jsonSumTypeOptions
  ) where

import Relude

import Data.Aeson
  (Options(..), SumEncoding(UntaggedValue), ToJSON, defaultOptions, encode)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Char (toLower)
import Text.Pretty.Simple (pStringNoColor)


-- | Converts data to 'Text'.
packJson :: ToJSON a => a -> Text
packJson = toStrict . pStringNoColor . decodeUtf8 . encode

-- | Options for JSON types using train case.
jsonCamelOptions :: Options
jsonCamelOptions = defaultOptions
  { omitNothingFields  = True
  , fieldLabelModifier = fieldLabelModifier $ aesonPrefix camelCase
  }

-- | Options for JSON sum types.
jsonSumTypeOptions :: Options
jsonSumTypeOptions = defaultOptions
  { sumEncoding            = UntaggedValue
  , constructorTagModifier = map toLower
  }
