-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Tests for search endpoints and related staff.
module Test.Search
  ( searchTests
  ) where

import Relude

import Servant.Auth.Client (Token(..))
import Servant.Client.Core (RunClient)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)

import Core.Sender (Phone(..))
import Union.App.Env (Env)
import Union.Search.Server (SearchEndpoints(..))
import Union.Server (Endpoints(..))

import Servant.Client
import Test.Hspec (shouldBe, shouldContain)
import Test.Mock (mkToken, rootClient, withClient)
import Union.Account.Profile.Server (ProfileEndpoints(..))
import Union.Account.Profile.Types (CreateInterestReq(..))
import Union.Account.Server (AccountEndpoints(..))
import Union.Search.Types (SuggestInterestsResp(..))


searchClient :: RunClient m => Token -> SearchEndpoints (AsClientT m)
searchClient = rootClient // eSearch

searchTests :: Env -> TestTree
searchTests env = testGroup
  "Search API"
  [ testCaseSteps "`../search/interests GET` returns at most 10 interests"
    $ \step -> do
        step "Preparing..."
        (_, token) <- mkToken env $ Phone "search1"
        forM_ ([1 .. 10] :: [Int]) $ \i -> withClient
          env
          (  _createInterest (profileClient token)
          .  CreateInterestReq
          $  "si"
          <> show i
          )

        step "Running..."
        interests <- si_respInterests <$> withClient
          env
          (searchClient token // _suggestInterests /: Nothing)
        length interests `shouldBe` 10
  --
  , testCaseSteps "`../search/interests?q=... GET` filters interests by ilike"
    $ \step -> do
        step "Preparing..."
        (_, token) <- mkToken env $ Phone "search2"
        let req = CreateInterestReq "interest for search"
        interest <- withClient
          env
          (profileClient token // _createInterest /: req)

        step "Running..."
        interests <- si_respInterests <$> withClient
          env
          (searchClient token // _suggestInterests /: Just "for search")
        interests `shouldContain` [interest]
  ]
  where
    profileClient :: RunClient m => Token -> ProfileEndpoints (AsClientT m)
    profileClient = rootClient // eAccount // _profile

