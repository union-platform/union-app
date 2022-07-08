-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

import Relude

import Test.Tasty (defaultMain, testGroup)

import Test.Account (accountTests)
import Test.Mock (initMockApp, mockEnv)
import Test.Swagger (swaggerTests)


main :: IO ()
main = do
  env <- mockEnv
  initMockApp env

  swagger <- swaggerTests
  defaultMain $ testGroup "Union tests" [swagger, accountTests env]
