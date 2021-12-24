-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | General-purpose generators.
module Test.Gen
  ( genInt
  , genText
  , genSeconds
  ) where

import Relude

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog (Gen)

import Core.Time (Seconds (..))


genInt :: Gen Int
genInt = Gen.enumBounded

genText :: Gen Text
genText = Gen.text (Range.constant 1 100) Gen.alphaNum

genSeconds :: Gen Seconds
genSeconds = Seconds <$> Gen.int (Range.constant 2 100)
