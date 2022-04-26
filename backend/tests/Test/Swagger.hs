-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Swagger doc tests.
module Test.Swagger
  ( swaggerTests
  ) where

import Relude

import Servant.OpenApi.Test (validateEveryToJSON)
import Test.Hspec (describe)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

import Union.Server (api)

import Test.Arbitrary ()


swaggerTests :: IO TestTree
swaggerTests = testSpec "Swagger" $ do
  describe "ToJSON matches ToSchema" $ validateEveryToJSON api
