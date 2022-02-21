-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: MPL-2.0

-- | This module implements the main monad application.
--
-- It should be used in the following way:
-- 1. Create your custom environment data type.
-- 2. Implement 'Core.Has.Has' instances for your environment.
-- 3. Specialize 'App' to your environment.
-- 4. Implement desired effects for your specialized version of monad.
module Core.Monad
  ( App(..)
  , runApp
  , runAppAsIO
  ) where

import Relude
import Relude.Extra.Bifunctor (firstF)

import Control.Exception (catch, throwIO, try)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Prometheus (MonadMonitor(..))

import Core.Error (AppException(..), ErrorWithSource)


-- | Main application monad. It has the following type variables:
-- * @err@: phantom type variable that represents type of errors thrown by 'App'
-- * @env@: application environment that stores settings and in-memory caches
-- * @a@: monadic result
newtype App (err :: Type) env a = App { getApp :: ReaderT env IO a }
  deriving newtype (Functor, Applicative)
  deriving newtype (Monad, MonadIO, MonadUnliftIO, MonadFail, MonadReader env)

-- | This instance allows to throw and catch errors that are visible in type
-- definitions. The implementation relies on underlying 'IO' machinery.
--
-- Use 'Core.Error.throwError' and 'Core.Error.catchError': these
-- functions automatically attach source code positions to errors.
instance (Show err, Typeable err)
  => MonadError (ErrorWithSource err) (App err env) where
  throwError :: ErrorWithSource err -> App err env a
  throwError = liftIO . throwIO . AppException
  {-# INLINE throwError #-}

  catchError
    :: App err env a -> (ErrorWithSource err -> App err env a) -> App err env a
  catchError action handler = App $ ReaderT $ \env -> do
    let ioAction = runApp env action
    ioAction `catch` \(AppException e) -> runApp env $ handler e
  {-# INLINE catchError #-}

-- | This instance is required for the @prometheus-client@ library.
instance MonadMonitor (App err env) where
  doIO :: IO () -> App err env ()
  doIO = liftIO
  {-# INLINE doIO #-}

-- | Run application by providing environment.
-- Throws 'AppException' if application has unhandled 'throwError'. Use
-- 'runAppAsIO' to handle exceptions as well.
runApp :: env -> App err env a -> IO a
runApp env = usingReaderT env . getApp
{-# INLINE runApp #-}

-- | Like 'runApp' but also catches 'AppException' and unwraps 'ErrorWithSource'
-- from it. Use this function to handle errors outside 'App' monad.
runAppAsIO
  :: (Show err, Typeable err)
  => env
  -> App err env a
  -> IO (Either (ErrorWithSource err) a)
runAppAsIO env = firstF getAppException . try . runApp env
{-# INLINE runAppAsIO #-}
