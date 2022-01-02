-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: MPL-2.0

-- | The main module that reexports all functionality.
module Core
  ( module Core.Db
    -- "Core.Db" contains useful helpers to work with DB. Also introduces
    -- abstractions and functions for 'Hasql.Pool.Pool'.
  , module Core.Error
    -- "Core.Error" contains types ans functions for the app error customization.
  , module Core.Has
    -- "Core.Has" contains functions and type clases to work with the environment
    -- data type.
  , module Core.Json
    -- "Core.Json" provides tools to work with JSON.
  , module Core.Jwt
    -- "Core.Jwt" provides convenient wrappers around JWT provided by the @jwt@
    -- library.
  , module Core.Measure
    -- "Core.Measure" contains useful type classes for dealing with the
    -- application metrics.
  , module Core.Monad
    -- "Core.Monad" contains the main monad around which your application
    -- should be build.
  , module Core.Random
    -- "Core.Random" contains functions to work with randomness.
  , module Core.Servant
    -- "Core.Servant" contains useful type alias to work with @servant@ and
    -- generic servant approach.
  , module Core.Swagger
    -- "Core.Swagger" contains helper functions to create @swagger@ instances.
  , module Core.Time
    -- "Core.Time" contains helper functions to work with time.
  ) where

import Core.Db
import Core.Error
import Core.Has
import Core.Json
import Core.Jwt
import Core.Measure
import Core.Monad
import Core.Random
import Core.Servant
import Core.Swagger
import Core.Time
