-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

import Relude

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.Hedgehog (fromGroup)

import Test.Jwt (jwtTests)


main :: IO ()
main = defaultMain properties

properties :: TestTree
properties = testGroup "Union tests"
  [ fromGroup jwtTests
  ]


