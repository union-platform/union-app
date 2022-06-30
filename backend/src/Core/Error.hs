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

    -- * Helper functions
  , toAppError
  , toLogMsg
  , throwOnNothing
  , throwOnNothingM
  , showErr
  ) where

import Relude
import Relude.Extra (firstF)

import qualified Control.Monad.Except as E (catchError, liftEither, throwError)

import Control.Monad.Except (MonadError)
import GHC.Stack.Types (CallStack(EmptyCallStack))
import Text.Pretty.Simple (pShow)

import Core.Logger (Msg(..), Severity)


-- | Type alias for errors that has access to 'CallStack'.
type WithError err m = (MonadError (ErrorWithSource err) m, HasCallStack)

-- | Wrapper around error type with attached source code position.
data ErrorWithSource err = ErrorWithSource
  { ewsSourcePosition :: !CallStack
  , ewsLogSeverity    :: !Severity
  , ewsError          :: !err
  }
  deriving stock (Show, Functor)

-- | Specialized version of 'E.throwError' that attaches source code position of
-- the place where this error was thrown.
throwError :: WithError err m => Severity -> err -> m a
throwError severity = E.throwError . ErrorWithSource callStack severity
{-# INLINE throwError #-}

-- | Specialized version of 'E.catchError'.
catchError :: WithError err m => m a -> (err -> m a) -> m a
catchError action handler = action `E.catchError` (handler . ewsError)
{-# INLINE catchError #-}

-- | Lift errors from 'Either' by rethrowing them with attached source position.
liftError :: WithError e m => Severity -> Either e a -> m a
liftError severity = either (throwError severity) pure
{-# INLINE liftError #-}

-- | Exception wrapper around 'ErrorWithSource'. Useful when you need to
-- throw/catch 'ErrorWithSource' as 'Exception'.
newtype AppException err = AppException
  { getAppException :: ErrorWithSource err }
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Helper to convert @err@ into something that can be thrown when you don't
-- have the ability to specify the 'SourcePosition'.
toNoSourceException :: Severity -> err -> AppException err
toNoSourceException severity =
  AppException . ErrorWithSource EmptyCallStack severity
{-# INLINE toNoSourceException #-}

-- | Converts @err'@ to @err@.
toAppError
  :: WithError err m
  => (err' -> err)
  -> ExceptT (ErrorWithSource err') m a
  -> m a
toAppError f action = firstF (fmap f) (runExceptT action) >>= E.liftEither

-- | Converts 'ErrorWithSource' to 'Msg' for logging purpose.
toLogMsg :: Show err => ErrorWithSource err -> Msg Severity
toLogMsg ErrorWithSource {..} = Msg
  { msgSeverity = ewsLogSeverity
  , msgStack    = ewsSourcePosition
  , msgText     = showErr ewsError
  }

-- | Extract the value from a maybe, throwing the given @err@ if the value
-- does not exist.
throwOnNothing :: WithError err m => Severity -> err -> Maybe a -> m a
throwOnNothing severity err =
  withFrozenCallStack (maybe (throwError severity err) pure)
{-# INLINE throwOnNothing #-}

-- - | Extract the value from a 'Maybe' in @m@, throwing the given @err@ if
-- the value does not exist.
throwOnNothingM :: WithError err m => Severity -> err -> m (Maybe a) -> m a
throwOnNothingM severity err action =
  withFrozenCallStack (action >>= throwOnNothing severity err)
{-# INLINE throwOnNothingM #-}

-- | Converts @err@ to 'Text' with pretty formatting.
showErr :: Show err => err -> Text
showErr = toStrict . pShow
{-# INLINE showErr #-}
