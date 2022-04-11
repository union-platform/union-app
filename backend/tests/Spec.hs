-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

import Relude

import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.Jwt (jwtTests)
import Test.Mock (MockEnv, mockEnv)


main :: IO ()
main = mockEnv >>= defaultMain . properties

properties :: MockEnv -> TestTree
properties env = testGroup "Union tests" [jwtTests env]
