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

import Core.Db (Id(..))
import Core.Sender (ConfirmationCode(..), Phone(..))
import Union.Account.Profile.Schema (Interest(..))
import Union.Account.Profile.Types
  (CreateInterestReq(..), CreateProfileReq(..), UpdateInterestsReq(..))
import Union.Account.SignIn.Types
  (AuthenticateReq(..), AuthenticateResp(..), RequestCodeReq(..), UserAgent(..))
import Union.Auth (JwtToken, mkJwtToken)
import Union.Search.Types (SuggestInterestsResp(..))


deriving newtype instance Arbitrary (Id a)
deriving newtype instance Arbitrary Phone
deriving newtype instance Arbitrary ConfirmationCode
deriving newtype instance Arbitrary UserAgent

instance Arbitrary JwtToken where
  arbitrary = mkJwtToken <$> arbitrary

instance Arbitrary RequestCodeReq where
  arbitrary = RequestCodeReq <$> arbitrary

instance Arbitrary AuthenticateReq where
  arbitrary = AuthenticateReq <$> arbitrary <*> arbitrary

instance Arbitrary AuthenticateResp where
  arbitrary = AuthenticateResp <$> arbitrary

instance Arbitrary CreateProfileReq where
  arbitrary = CreateProfileReq <$> arbitrary

instance Arbitrary CreateInterestReq where
  arbitrary = CreateInterestReq <$> arbitrary

instance Arbitrary UpdateInterestsReq where
  arbitrary = UpdateInterestsReq <$> arbitrary

instance f ~ Identity => Arbitrary (Interest f) where
  arbitrary = Interest <$> arbitrary <*> arbitrary

instance Arbitrary SuggestInterestsResp where
  arbitrary = SuggestInterestsResp <$> arbitrary
