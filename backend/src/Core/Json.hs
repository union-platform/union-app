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
  (Options(..), SumEncoding(UntaggedValue), ToJSON, defaultOptions)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Char (toLower)


-- | Converts data to 'Text'.
packJson :: ToJSON a => a -> Text
packJson = decodeUtf8 . encodePretty

-- | Options for JSON types using train case.
jsonCamelOptions :: Options
jsonCamelOptions = defaultOptions
  { omitNothingFields = True
  , fieldLabelModifier = fieldLabelModifier $ aesonPrefix camelCase
  }

-- | Options for JSON sum types.
jsonSumTypeOptions :: Options
jsonSumTypeOptions = defaultOptions
  { sumEncoding = UntaggedValue
  , constructorTagModifier = map toLower
  }
