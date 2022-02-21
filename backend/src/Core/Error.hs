-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: MPL-2.0

-- | This module contains types ans functions for the app error customization.
-- After specializing 'WithError' we can throw and catch pure errors that also
-- have source code position attached to them automatically.
module Core.Error
  ( -- * Pure errors handling
    WithError
  , ErrorWithSource(..)
  , throwError
  , catchError
  , liftError

    -- * Exceptions
  , AppException(..)
  , toNoSourceException

    -- * Helper unctions
  , toAppError
  , throwOnNothing
  , throwOnNothingM
  , showErr

    -- * 'SourcePosition' helpers
  , SourcePosition(..)
  , toSourcePosition
  ) where

import Relude
import Relude.Extra (firstF)

import qualified Control.Monad.Except as E (catchError, liftEither, throwError)

import Control.Monad.Except (MonadError)
import GHC.Stack (SrcLoc(SrcLoc, srcLocModule, srcLocStartLine))
import Text.Pretty.Simple (pShow)


-- | Type alias for errors that has access to 'CallStack'.
type WithError err m = (MonadError (ErrorWithSource err) m, HasCallStack)

-- | Wrapper around error type with attached source code position.
data ErrorWithSource err = ErrorWithSource
  { errorWithSourceCallStack :: !SourcePosition
  , errorWithSourceType      :: !err
  }
  deriving stock (Show, Eq, Functor)

-- | Specialized version of 'E.throwError' that attaches source code position of
-- the place where this error was thrown.
throwError :: WithError err m => err -> m a
throwError = E.throwError . ErrorWithSource (toSourcePosition callStack)
{-# INLINE throwError #-}

-- | Specialized version of 'E.catchError'.
catchError :: WithError err m => m a -> (err -> m a) -> m a
catchError action handler =
  action `E.catchError` (handler . errorWithSourceType)
{-# INLINE catchError #-}

-- | Lift errors from 'Either' by rethrowing them with attached source position.
liftError :: WithError e m => Either e a -> m a
liftError = either throwError pure
{-# INLINE liftError #-}

-- | Formatted source code position. See 'toSourcePosition' for more details.
newtype SourcePosition = SourcePosition { unSourcePosition :: Text }
  deriving newtype (Show, Eq)

-- | Display 'CallStack' as 'SourcePosition' in the following format:
-- @
-- Module.function#line_number
-- @
toSourcePosition :: CallStack -> SourcePosition
toSourcePosition cs = SourcePosition showCallStack
  where
    showCallStack :: Text
    showCallStack = case getCallStack cs of
      [] -> "<unknown loc>"
      [(name, loc)] -> showLoc name loc
      (_, loc) : (callerName, _) : _ -> showLoc callerName loc

    showLoc :: String -> SrcLoc -> Text
    showLoc name SrcLoc {..} =
      toText srcLocModule <> "." <> toText name <> "#" <> show srcLocStartLine

-- | Exception wrapper around 'ErrorWithSource'. Useful when you need to
-- throw/catch 'ErrorWithSource' as 'Exception'.
newtype AppException err = AppException
  { getAppException :: ErrorWithSource err }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Helper to convert @err@ into something that can be thrown when you don't
-- have the ability to specify the 'SourcePosition'.
toNoSourceException :: err -> AppException err
toNoSourceException =
  AppException . ErrorWithSource (SourcePosition "<unknown loc>")
{-# INLINE toNoSourceException #-}

-- | Converts @err'@ to @err@.
toAppError
  :: WithError err m
  => (err' -> err)
  -> ExceptT (ErrorWithSource err') m a
  -> m a
toAppError f action = firstF (fmap f) (runExceptT action) >>= E.liftEither

-- | Extract the value from a maybe, throwing the given @err@ if the value
-- does not exist.
throwOnNothing :: WithError err m => err -> Maybe a -> m a
throwOnNothing err = withFrozenCallStack . maybe (throwError err) pure
{-# INLINE throwOnNothing #-}

-- - | Extract the value from a 'Maybe' in @m@, throwing the given @err@ if
-- the value does not exist.
throwOnNothingM :: WithError err m => err -> m (Maybe a) -> m a
throwOnNothingM err action =
  withFrozenCallStack $ action >>= throwOnNothing err
{-# INLINE throwOnNothingM #-}

-- | Converts @err@ to 'Text' with pretty formatting.
showErr :: Show err => err -> Text
showErr = toStrict . pShow
{-# INLINE showErr #-}
