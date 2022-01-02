-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Main
  ( main
  ) where

import Relude

import Main.Utf8 (withUtf8)

import Union (runUnion, union, unionOpts)


main :: IO ()
main = withUtf8 $ do
  opts <- unionOpts
  runUnion opts union
