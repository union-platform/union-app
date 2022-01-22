-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Main
  ( main
  ) where

import Relude

import Main.Utf8 (withUtf8)

import Command (Arguments(..), Command(..), getArguments)
import Command.Migrations (migrations)
import Union (union)


main :: IO ()
main = withUtf8 $ do
  Arguments {..} <- getArguments
  case aCommand of
    Run            -> union aConfig
    Migrations cmd -> migrations aConfig cmd
