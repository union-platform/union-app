-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: MPL-2.0

-- | This module contains the 'MonadMeasure' type class which purpose is to
-- apply some metric calculations on actions, e.g. taken time.
module Core.Measure
  ( MonadMeasure
  , MonadTimed(..)

    -- * Environment types
  , Timings
  , PrometheusRegistry

    -- * Internals
  , timedActionImplWith
  , timedActionPrometheusImpl
  ) where

import Relude

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified System.Metrics as Metrics
import qualified System.Metrics.Distribution as Distribution

import Prometheus
  ( Histogram
  , Info(..)
  , MonadMonitor
  , defaultBuckets
  , histogram
  , observe
  , register
  )
import Relude.Extra.CallStack (ownName)
import System.CPUTime (getCPUTime)
import System.Metrics.Distribution (Distribution)

import Core.Has (Has, grab)


-- | This type class is used to perform action and measure time and other
-- metrics needed to be performed.
class Monad m => MonadTimed m where
  timedAction :: HasCallStack => m a -> m a

-- | A type alias for map of 'Distribution's.
type Timings = IORef (Map Text Distribution)

-- | A type alias for map of 'Histogram' for @prometheus@ metrics.
type PrometheusRegistry = IORef (HashMap Text Histogram)

-- | Constraint for the monadic actions that have access to measurements.
type MonadMeasure m = (HasCallStack, MonadTimed m)

-- | Basic implementation of 'timedAction' function for 'MonadTimed' instances.
-- It measures the time taken to perform the given action and store it in the
-- @timings@ distribution with the label of the action.
-- It also receives any function that work with metrics. This function can use
-- label and time data inside.
--
-- __Usage examples:__
-- @
-- __instance__ 'MonadTimed' App __where__
--   'timedAction' = 'timedActionImplWith' myMetrics \"MY_APP\"
--     __where__
--       myMetrics :: 'Text' -> 'Double' -> App ()
--       myMetrics label time = __do__
--         prometheusMetrics label time
--         otherMetrics label time
-- @
timedActionImplWith
  :: forall r m a
   . ( MonadReader r m
     , Has Timings r
     , Has Metrics.Store r
     , MonadIO m
     , HasCallStack
     )
  => (Text -> Double -> m ())
  -- ^ any given metrics collection actions.
  -> Text
  -- ^ Global name
  -> m a
  -- ^ Action
  -> m a
timedActionImplWith metricsAct _nm action = do
  start   <- liftIO getCPUTime
  !result <- action
  end     <- liftIO getCPUTime
  let !timeTaken = fromIntegral (end - start) * 1e-12
  dist <- getOrCreateDistribution $ toText ownName
  liftIO $ Distribution.add dist timeTaken
  metricsAct (toText ownName) timeTaken
  pure result
  where
    getOrCreateDistribution :: Text -> m Distribution
    getOrCreateDistribution label = do
      timingsRef <- grab @Timings
      store      <- grab @Metrics.Store
      liftIO $ do
        distMap <- readIORef timingsRef
        whenNothing (Map.lookup label distMap) $ do
          newDist <- Metrics.createDistribution label store
          modifyIORef' timingsRef (Map.insert label newDist)
          return newDist

-- | Helper function to be used in instance implementations.
-- It measures the time taken to perform the given action and store it
-- in the @timings@ distribution with the label of the action.
-- This is implementation has the integration with @prometheus@.
timedActionPrometheusImpl
  :: forall r m a
   . ( MonadReader r m
     , Has Timings r
     , Has Metrics.Store r
     , Has PrometheusRegistry r
     , MonadIO m
     , HasCallStack
     , MonadMonitor m
     )
  => Text
  -- ^ Global name
  -> m a
  -- ^ Action
  -> m a
timedActionPrometheusImpl nm = timedActionImplWith registerWithPrometheus nm
  where
    registerWithPrometheus :: Text -> Double -> m ()
    registerWithPrometheus label reading = do
      promRef <- grab @PrometheusRegistry
      promMap <- readIORef promRef
      case HashMap.lookup label promMap of
        Just hist -> observe hist reading
        Nothing   -> do
          newHist <- register
            $ histogram (Prometheus.Info nm label) defaultBuckets
          void $ atomicModifyIORef'
            promRef
            (\pHashMap -> (HashMap.insert label newHist pHashMap, ()))
          observe newHist reading
