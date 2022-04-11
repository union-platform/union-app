-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module provides tools to work with JSON.
module Core.Json
  ( packJson
  , JsonDataOptions
  , jsonSumTypeOptions
  ) where

import Relude

import Data.Aeson
  (Options(..), SumEncoding(UntaggedValue), ToJSON, defaultOptions, encode)
import Data.Char (toLower)
import Deriving.Aeson
  (FieldLabelModifier, OmitNothingFields, StringModifier(..), StripPrefix)
import Text.Pretty.Simple (pStringNoColor)


-- | Converts data to 'Text'.
packJson :: ToJSON a => a -> Text
packJson = toStrict . pStringNoColor . decodeUtf8 . encode

-- | Apply function to first 'Char' in 'String'.
applyFirst :: (Char -> Char) -> String -> String
applyFirst _ []       = []
applyFirst f [x     ] = [f x]
applyFirst f (x : xs) = f x : xs

-- | 'StringModifier' for camelCase.
data CamelCase

instance StringModifier CamelCase where
  getStringModifier = applyFirst toLower

-- | Options for Union JSON data types.
type JsonDataOptions str
  = '[OmitNothingFields , FieldLabelModifier '[StripPrefix str , CamelCase]]

-- | Options for JSON sum types.
jsonSumTypeOptions :: Options
jsonSumTypeOptions = defaultOptions
  { sumEncoding            = UntaggedValue
  , constructorTagModifier = map toLower
  }
