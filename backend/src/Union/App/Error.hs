-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module describes Union error types.
module Union.App.Error
  ( Error(..)
  ) where

import Relude

import Hasql.Pool (UsageError)
import Network.HTTP.Types.Header (HeaderName)


-- | Errors in Union.
data Error
  = NotFound
  -- ^ General not found.

  | ApiError Text
  -- ^ Some exceptional circumstance has happened to stop execution and return.
  -- Optional text to provide some context in server logs.

  | NotAllowed Text
  -- ^ A required permission level was not met.
  -- Optional text to provide some context.

  | Invalid Text
  -- ^ Given inputs do not conform to the expected format or shape.
  -- Optional text to provide some context in server logs.

  | MissingHeader HeaderName
  -- ^ Some header expected, but not present in header list.

  | HeaderDecodeError Text
  -- ^ An authentication header that was required was provided but not in a
  -- format that the server can understand.

  | DbError (Maybe UsageError)
  -- ^ Database specific errors.
  deriving stock (Show, Eq)
