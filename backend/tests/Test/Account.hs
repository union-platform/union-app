-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Tests for account endpoints and related staff.
module Test.Account
  ( accountTests
  ) where

import Relude

import qualified Rel8 as Sql

import Network.HTTP.Types.Status
  ( badRequest400
  , expectationFailed417
  , internalServerError500
  , tooManyRequests429
  , unauthorized401
  )
import Rel8 ((==.), Result)
import Servant.Auth.Client (Token(..))
import Servant.Client.Core (RunClient)
import Servant.Client.Core.HasClient ((//), (/:))
import Servant.Client.Generic (AsClientT)
import Test.Hspec (expectationFailure, shouldBe, shouldThrow)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, testCaseSteps)

import Core.Sender (ConfirmationCode(..), Phone(..))
import Union.Account.Profile.Schema (Interest(..), Profile(..))
import Union.Account.Profile.Server (ProfileEndpoints(..))
import Union.Account.Profile.Service (findProfile, getUserInterests)
import Union.Account.Profile.Types
  (CreateInterestReq(..), CreateProfileReq(..), UpdateInterestsReq(..))
import Union.Account.Schema (Account(..))
import Union.Account.Server (AccountEndpoints(..))
import Union.Account.Service (findAccount)
import Union.Account.SignIn.Schema
  (AuthLog(..), Confirmation(..), authLogSchema, confirmationSchema)
import Union.Account.SignIn.Server (SignInEndpoints(..))
import Union.Account.SignIn.Types
  (AuthenticateReq(..), AuthenticateResp(..), RequestCodeReq(..))
import Union.Account.Types (mkUserName)
import Union.App.Db (executeS, selectOne)
import Union.App.Env (Env)
import Union.Auth (mkJwtToken, unJwtToken)
import Union.Server (Endpoints(..))

import Test.Mock
  (MockApp, apiStatusCode, mkToken, rootClient, runMockApp, withClient)


accountClient :: RunClient m => AccountEndpoints (AsClientT m)
accountClient = rootClient // eAccount

accountTests :: Env -> TestTree
accountTests env =
  testGroup "Account API ../accounts" [signInTests env, profileTests env]

signInTests :: Env -> TestTree
signInTests env = testGroup
  "SignIn API"
  [ testCase "`../signIn/request POST` throws error for invalid phone" $ do
    let
      phones =
        [ -- phone should start with + symbol
          Phone "999999999999"
          -- phone should contain at least 11 digits
        , Phone "+9999999999"
          -- phone should contain not more then 16 digits
        , Phone "+999999999999999999"
        ]

    forM_ phones $ \phone ->
      withClient env (signInClient // _requestCode /: RequestCodeReq phone)
        `shouldThrow` apiStatusCode badRequest400
  --
  , testCase "`../signIn/request POST` creates account for new phone" $ do
    let
      phone = Phone "+12345678901"
      req   = RequestCodeReq phone
    void $ withClient env (signInClient // _requestCode /: req)
    runMockApp env $ findAccount phone >>= \case
      Nothing -> liftIO $ expectationFailure "Cannot find account in DB"
      Just Account {..} -> liftIO $ aPhone `shouldBe` phone
  --
  , testCase "`../signIn/request POST` throttling too many requests" $ do
    let req = RequestCodeReq $ Phone "+12345678902"
    void $ withClient env (signInClient // _requestCode /: req)
    withClient env (signInClient // _requestCode /: req)
      `shouldThrow` apiStatusCode tooManyRequests429
  --
  , testCase "`../signIn POST` throws error for unknown phone" $ do
    let
      req = AuthenticateReq
        { a_reqPhone = Phone "+99999999999"
        , a_reqCode  = ConfirmationCode "123456"
        }
    withClient env (signInClient // _authenticate /: Nothing /: req)
      `shouldThrow` apiStatusCode unauthorized401
    assertBool "True" True
  --
  , testCase "`../signIn POST` throws error for wrong code" $ do
    let
      phone = Phone "+12345678903"
      req1  = RequestCodeReq phone
      req2  = AuthenticateReq
        { a_reqPhone = phone
        , a_reqCode  = ConfirmationCode "wrong"
        }
    void $ withClient env (signInClient // _requestCode /: req1)
    withClient env (signInClient // _authenticate /: Nothing /: req2)
      `shouldThrow` apiStatusCode unauthorized401
  --
  , testCase "`../signIn POST` returns JWT for valid phone and code" $ do
    let
      phone = Phone "+12345678904"
      req1  = RequestCodeReq phone
    void $ withClient env (signInClient // _requestCode /: req1)
    code <- runMockApp env $ findAccount phone >>= findCode >>= \case
      Nothing   -> pure $ ConfirmationCode "wrong"
      Just code -> pure code
    let req2 = AuthenticateReq { a_reqPhone = phone, a_reqCode = code }
    AuthenticateResp {..} <- withClient
      env
      (signInClient // _authenticate /: Nothing /: req2)
    a_respToken `shouldBe` mkJwtToken (unJwtToken a_respToken)
  --
  , testCase "`../signIn POST` creates auth log entry" $ do
    let
      phone = Phone "+12345678905"
      req1  = RequestCodeReq phone
    void $ withClient env (signInClient // _requestCode /: req1)
    account <- runMockApp env $ findAccount phone
    code    <- runMockApp env $ findCode account >>= \case
      Nothing   -> pure $ ConfirmationCode "wrong"
      Just code -> pure code
    let req2 = AuthenticateReq { a_reqPhone = phone, a_reqCode = code }
    void $ withClient env (signInClient // _authenticate /: Nothing /: req2)
    runMockApp env $ findAuthLog account >>= \case
      Nothing -> liftIO $ expectationFailure "Cannot find auth log in DB"
      Just AuthLog {..} -> liftIO $ alCode `shouldBe` code
  ]
  where
    signInClient :: RunClient m => SignInEndpoints (AsClientT m)
    signInClient = accountClient // _signIn

profileTests :: Env -> TestTree
profileTests env = testGroup
  "Profile API"
  [ testCaseSteps "`../profile POST` throws error for invalid name" $ \step ->
    do
      step "Preparing..."
      (_, token) <- mkToken env $ Phone "profile1"

      let
        names =
          [ -- name should be at least 4 symbols
            "abc"
            -- name should be at most 32 symbols
          , "abc abc abc abc abc abc abc abc abc abc abc abc abc"
            -- name cannot contain digits
          , "abc 8"
            -- name cannot contain special symbols
          , "abc %"
          ]

      step "Running..."
      forM_ names $ \name ->
        withClient
            env
            (profileClient token // _createProfile /: CreateProfileReq name)
          `shouldThrow` apiStatusCode expectationFailed417
  --
  , testCaseSteps "`../profile POST` creates profile for account" $ \step -> do
    step "Preparing..."
    (aId, token) <- mkToken env $ Phone "profile2"
    let
      name = "Lev Tolstoy"
      req  = CreateProfileReq name

    step "Running..."
    void $ withClient env (profileClient token // _createProfile /: req)
    runMockApp env $ findProfile aId >>= \case
      Nothing -> liftIO $ expectationFailure "Cannot find profile in DB"
      Just Profile {..} -> liftIO $ Just pName `shouldBe` mkUserName name
  --
  , testCaseSteps "`../profile POST` throws error for existing profile"
    $ \step -> do
        step "Preparing..."
        (_, token) <- mkToken env $ Phone "profile3"
        let req = CreateProfileReq "Vasya"
        void $ withClient env (profileClient token // _createProfile /: req)

        step "Running..."
        withClient env (profileClient token // _createProfile /: req)
          `shouldThrow` apiStatusCode badRequest400
        assertBool "True" True
  --
  , testCaseSteps "`../profile/interests POST` creates new interest" $ \step ->
    do
      step "Preparing..."
      let name = "interests1"
      (_, token) <- mkToken env $ Phone name
      let req = CreateInterestReq name

      step "Running..."
      Interest {..} <- withClient
        env
        (profileClient token // _createInterest /: req)
      iName `shouldBe` name
  --
  , testCaseSteps
      "`../profile/interests POST` throws error for existing interest"
    $ \step -> do
        step "Preparing..."
        let name = "interests2"
        (_, token) <- mkToken env $ Phone name
        let req = CreateInterestReq name
        void $ withClient env (profileClient token // _createInterest /: req)

        step "Running..."
        withClient env (profileClient token // _createInterest /: req)
          `shouldThrow` apiStatusCode internalServerError500
        assertBool "True" True
  --
  , testCaseSteps "`../profile/interests PUT` adds user interests" $ \step -> do
    step "Preparing..."
    (aId, token) <- mkToken env $ Phone "interests3"
    i1           <- withClient
      env
      (profileClient token // _createInterest /: CreateInterestReq "i1")
    i2 <- withClient
      env
      (profileClient token // _createInterest /: CreateInterestReq "i2")
    i3 <- withClient
      env
      (profileClient token // _createInterest /: CreateInterestReq "i3")
    let
      req = UpdateInterestsReq [iInterestId i1, iInterestId i2, iInterestId i3]

    step "Running..."
    void $ withClient env (profileClient token // _updateInterests /: req)
    interests <- runMockApp env $ getUserInterests aId
    interests `shouldBe` [i1, i2, i3]
  --
  , testCaseSteps
      "`../profile/interests PUT` drops user interests which not in request"
    $ \step -> do
        step "Preparing..."
        (aId, token) <- mkToken env $ Phone "interests4"
        i1           <- withClient
          env
          (profileClient token // _createInterest /: CreateInterestReq "1i")
        i2 <- withClient
          env
          (profileClient token // _createInterest /: CreateInterestReq "2i")
        i3 <- withClient
          env
          (profileClient token // _createInterest /: CreateInterestReq "3i")
        let
          req =
            UpdateInterestsReq [iInterestId i1, iInterestId i2, iInterestId i3]

        step "Running..."
        void $ withClient env (profileClient token // _updateInterests /: req)
        void $ withClient
          env
          (profileClient token // _updateInterests /: UpdateInterestsReq [])
        interests <- runMockApp env $ getUserInterests aId
        interests `shouldBe` []
  ]
  where
    profileClient :: RunClient m => Token -> ProfileEndpoints (AsClientT m)
    profileClient = accountClient // _profile


-- | Helper to find 'ConfirmationCode' in DB.
findCode :: Maybe (Account Result) -> MockApp (Maybe ConfirmationCode)
findCode Nothing                       = pure Nothing
findCode (Just Account { aAccountId }) = executeS . selectOne $ do
  Confirmation { cAccountId, cCode } <- Sql.each confirmationSchema
  Sql.where_ $ cAccountId ==. Sql.lit aAccountId
  pure cCode

-- | Helper to find 'AuthLog' in DB.
findAuthLog :: Maybe (Account Result) -> MockApp (Maybe (AuthLog Result))
findAuthLog Nothing                       = pure Nothing
findAuthLog (Just Account { aAccountId }) = executeS . selectOne $ do
  authLog@AuthLog { alAccountId } <- Sql.each authLogSchema
  Sql.where_ $ alAccountId ==. Sql.lit aAccountId
  pure authLog
