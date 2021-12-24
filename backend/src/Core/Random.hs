-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: MPL-2.0

-- | Utilities for generating random strings.
module Core.Random
  ( mkRandomDigits
  , mkRandomString
  ) where

import Relude
import Relude.Unsafe ((!!))

import System.Random (newStdGen, randomRIO, randomRs)


-- | Generates @n@ random digits.
mkRandomDigits :: MonadIO m => Int -> m Text
mkRandomDigits n = toText . take n . randomRs ('0', '9') <$> liftIO newStdGen

-- | Make a random string of a given @n@ length, generated from following chars:
-- 1. Lowercase characters @[a..z]@
-- 2. Uppercase characters @[A..Z]@
-- 3. Digits @[0..9]@.
-- Returns empty string if given length is less than zero.
mkRandomString :: MonadIO m => Int -> m Text
mkRandomString n = liftIO $ toText <$> replicateM n peekRandomChar
  where
    alphabet :: String
    alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

    alphabetLength :: Int
    alphabetLength = length alphabet

    peekRandomChar :: IO Char
    peekRandomChar = do
      i <- randomRIO (0, alphabetLength - 1)
      pure $ alphabet !! i
