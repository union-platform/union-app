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
  (badRequest400, expectationFailed417, tooManyRequests429, unauthorized401)
import Rel8 ((==.), Result)
import Servant.Auth.Client (Token(..))
import Servant.Client.Core (RunClient)
import Servant.Client.Core.HasClient ((//))
import Servant.Client.Generic (AsClientT)
import Test.Hspec (expectationFailure, shouldBe, shouldThrow)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, testCaseSteps)

import Core.Sender (ConfirmationCode(..), Phone(..))
import Union.Account.Profile.Schema (Profile(..))
import Union.Account.Profile.Server (ProfileEndpoints(..))
import Union.Account.Profile.Service (findProfile)
import Union.Account.Profile.Types (CreateProfileReq(..))
import Union.Account.Schema (Account(..), AccountId)
import Union.Account.Server (AccountEndpoints(..))
import Union.Account.Service (createAccount, findAccount)
import Union.Account.SignIn.Schema
  (AuthLog(..), Confirmation(..), authLogSchema, confirmationSchema)
import Union.Account.SignIn.Server (SignInEndpoints(..))
import Union.Account.SignIn.Types
  (AuthenticateReq(..), AuthenticateResp(..), RequestCodeReq(..))
import Union.Account.Types (mkUserName)
import Union.App.Db (executeS, selectOne)
import Union.App.Env (Env)
import Union.Auth (generateJwtToken, mkJwtToken, unJwtToken)
import Union.Server (Endpoints(..))

import Test.Mock (MockApp, apiStatusCode, rootClient, runMockApp, withClient)


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
      withClient env (_requestCode signInClient $ RequestCodeReq phone)
        `shouldThrow` apiStatusCode badRequest400
  --
  , testCase "`../signIn/request POST` creates account for new phone" $ do
    let
      phone = Phone "+12345678901"
      req   = RequestCodeReq phone
    void $ withClient env (_requestCode signInClient req)
    runMockApp env $ findAccount phone >>= \case
      Nothing -> liftIO $ expectationFailure "Cannot find account in DB"
      Just Account {..} -> liftIO $ aPhone `shouldBe` phone
  --
  , testCase "`../signIn/request POST` throttling too many requests" $ do
    let req = RequestCodeReq $ Phone "+12345678902"
    void $ withClient env (_requestCode signInClient req)
    withClient env (_requestCode signInClient req)
      `shouldThrow` apiStatusCode tooManyRequests429
  --
  , testCase "`../signIn POST` throws error for unknown phone" $ do
    let
      req = AuthenticateReq
        { a_reqPhone = Phone "+99999999999"
        , a_reqCode  = ConfirmationCode "123456"
        }
    withClient env (_authenticate signInClient Nothing req)
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
    void $ withClient env (_requestCode signInClient req1)
    withClient env (_authenticate signInClient Nothing req2)
      `shouldThrow` apiStatusCode unauthorized401
  --
  , testCase "`../signIn POST` returns JWT for valid phone and code" $ do
    let
      phone = Phone "+12345678904"
      req1  = RequestCodeReq phone
    void $ withClient env (_requestCode signInClient req1)
    code <- runMockApp env $ findAccount phone >>= findCode >>= \case
      Nothing   -> pure $ ConfirmationCode "wrong"
      Just code -> pure code
    let req2 = AuthenticateReq { a_reqPhone = phone, a_reqCode = code }
    AuthenticateResp {..} <- withClient
      env
      (_authenticate signInClient Nothing req2)
    a_respToken `shouldBe` mkJwtToken (unJwtToken a_respToken)
  --
  , testCase "`../signIn POST` creates auth log entry" $ do
    let
      phone = Phone "+12345678905"
      req1  = RequestCodeReq phone
    void $ withClient env (_requestCode signInClient req1)
    account <- runMockApp env $ findAccount phone
    code    <- runMockApp env $ findCode account >>= \case
      Nothing   -> pure $ ConfirmationCode "wrong"
      Just code -> pure code
    let req2 = AuthenticateReq { a_reqPhone = phone, a_reqCode = code }
    void $ withClient env (_authenticate signInClient Nothing req2)
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
      (_, token) <- mkToken $ Phone "profile1"

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
            (_createProfile (profileClient token) $ CreateProfileReq name)
          `shouldThrow` apiStatusCode expectationFailed417
  --
  , testCaseSteps "`../profile POST` creates profile for account" $ \step -> do
    step "Preparing..."
    (aId, token) <- mkToken $ Phone "profile2"
    let
      name = "Lev Tolstoy"
      req  = CreateProfileReq name

    step "Running..."
    void $ withClient env (_createProfile (profileClient token) req)
    runMockApp env $ findProfile aId >>= \case
      Nothing -> liftIO $ expectationFailure "Cannot find profile in DB"
      Just Profile {..} -> liftIO $ Just pName `shouldBe` mkUserName name
  --
  , testCaseSteps "`../profile POST` throws error for existing profile"
    $ \step -> do
        step "Preparing..."
        (_, token) <- mkToken $ Phone "profile3"
        let req = CreateProfileReq "Vasya"
        void $ withClient env (_createProfile (profileClient token) req)

        step "Running..."
        withClient env (_createProfile (profileClient token) req)
          `shouldThrow` apiStatusCode badRequest400
        assertBool "True" True
  ]
  where
    profileClient :: RunClient m => Token -> ProfileEndpoints (AsClientT m)
    profileClient = accountClient // _profile

    mkToken :: Phone -> IO (AccountId, Token)
    mkToken phone = runMockApp env $ do
      aId <- aAccountId <$> createAccount phone
      (aId, ) . Token . toStrict . unJwtToken <$> generateJwtToken aId


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
