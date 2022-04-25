-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module provides tools to work with JSON.
module Core.Json
  ( packJson

    -- * Helpers to create JSON instances
  , JsonCamelCase
  , JsonSnakeCase
  , jsonSumTypeOptions
  ) where

import Relude

import Data.Aeson
  (Options(..), SumEncoding(UntaggedValue), ToJSON, defaultOptions, encode)
import Data.Char (toLower)
import Deriving.Aeson
  ( CamelToSnake
  , FieldLabelModifier
  , OmitNothingFields
  , StringModifier(..)
  , StripPrefix
  )
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
  getStringModifier :: String -> String
  getStringModifier = applyFirst toLower
  {-# INLINE getStringModifier #-}


-- | Options for Union JSON data types in camelCase.
type JsonCamelCase str
  = '[OmitNothingFields , FieldLabelModifier '[StripPrefix str , CamelCase]]

-- | Options for Union JSON data types in snake_case.
type JsonSnakeCase str
  = '[OmitNothingFields , FieldLabelModifier '[StripPrefix str , CamelToSnake]]

-- | Options for JSON sum types.
jsonSumTypeOptions :: Options
jsonSumTypeOptions = defaultOptions
  { sumEncoding            = UntaggedValue
  , constructorTagModifier = map toLower
  }
