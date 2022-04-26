-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module describes Union error types.
module Union.App.Error
  ( Error(..)
  , toHttpError
  ) where

import Relude

import Data.CaseInsensitive (foldedCase)
import Data.Text.Internal.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
import Hasql.Pool (UsageError)
import Network.HTTP.Types.Header (HeaderName, hRetryAfter)
import Servant (ServerError(..), err400, err401, err404, err417, err500)

import Core.Error (ErrorWithSource(..), showErr)
import Core.Time (Seconds(..))


-- | Errors in Union.
data Error
  = NotFound
  -- ^ General not found.

  | ApiError Text
  -- ^ Some exceptional circumstance has happened to stop execution and return.
  -- Optional text to provide some context in server logs.

  | ToManyRequests Seconds
  -- ^ Requests were sent to often, user needs to retry later.

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

  | BadRequest Text
  -- ^ The server cannot process the request because of a client error.
  deriving stock (Show, Eq)

-- | Map 'Error' into a HTTP error code.
toHttpError :: ErrorWithSource Error -> ServerError
toHttpError ErrorWithSource {..} = case ewsError of
  BadRequest msg -> err400 { errBody = encodeUtf8 msg }
  NotAllowed msg -> err401 { errBody = encodeUtf8 msg }
  MissingHeader name ->
    err401 { errBody = toLazy $ "Header not found: " <> foldedCase name }
  HeaderDecodeError name ->
    err401 { errBody = encodeUtf8 $ "Unable to decode header: " <> name }
  NotFound           -> err404
  Invalid        msg -> err417 { errBody = encodeUtf8 msg }
  ToManyRequests n   -> err429
    { errHeaders =
      [(hRetryAfter, encodeUtf8 . toLazyText . decimal $ getSeconds n)]
    }

  ApiError msg -> err500 { errBody = encodeUtf8 msg }
  DbError  e   -> err500 { errBody = encodeUtf8 $ showErr e }

-- | 'err429' Too Many Requests
err429 :: ServerError
err429 = ServerError
  { errHTTPCode     = 429
  , errReasonPhrase = "Too Many Requests"
  , errBody         = ""
  , errHeaders      = []
  }
