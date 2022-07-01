-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances required for tests.
module Test.Arbitrary
  () where

import Relude

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Instances ()

import Core.Jwt (JwtToken(..))
import Core.Sender (ConfirmationCode(..), Phone(..))
import Union.Account.Profile.Types (CreateProfileReq(..))
import Union.Account.SignIn.Types
  (AuthenticateReq(..), AuthenticateResp(..), RequestCodeReq(..), UserAgent(..))


deriving newtype instance Arbitrary Phone
deriving newtype instance Arbitrary ConfirmationCode
deriving newtype instance Arbitrary JwtToken
deriving newtype instance Arbitrary UserAgent

instance Arbitrary RequestCodeReq where
  arbitrary = RequestCodeReq <$> arbitrary

instance Arbitrary AuthenticateReq where
  arbitrary = AuthenticateReq <$> arbitrary <*> arbitrary

instance Arbitrary AuthenticateResp where
  arbitrary = AuthenticateResp <$> arbitrary

instance Arbitrary CreateProfileReq where
  arbitrary = CreateProfileReq <$> arbitrary
