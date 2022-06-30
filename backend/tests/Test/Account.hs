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
  (badRequest400, tooManyRequests429, unauthorized401)
import Rel8 ((==.), Result)
import Servant.Client.Core (RunClient)
import Servant.Client.Generic (AsClientT)
import Servant.Client.Core.HasClient ((//))
import Test.Hspec (expectationFailure, shouldBe, shouldThrow)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Core.Jwt (JwtToken(..))
import Core.Sender (ConfirmationCode(..), Phone(..))
import Union.Account.Schema
  ( Account(..)
  , AuthLog(..)
  , Confirmation(..)
  , authLogSchema
  , confirmationSchema
  )
import Union.Account.Server (AccountEndpoints(..))
import Union.Account.Service (findAccount)
import Union.Account.Types (RequestCodeReq(..), SignInReq(..), SignInResp(..))
import Union.App.Db (executeS, selectOne)
import Union.App.Env (Env)
import Union.Server (Endpoints(..))

import Test.Mock (MockApp, apiStatusCode, runMockApp, withClient, rootClient)


accountClient :: RunClient m => AccountEndpoints (AsClientT m)
accountClient = rootClient // eAccount

accountTests :: Env -> TestTree
accountTests env = testGroup
  "Account API"
  [ testCase "`POST /signIn/request` throws error for invalid phone" $ do
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
      withClient env (_requestCode accountClient $ RequestCodeReq phone)
        `shouldThrow` apiStatusCode badRequest400
  , testCase "`POST /signIn/request` creates account for new phone" $ do
    let
      phone = Phone "+12345678901"
      req   = RequestCodeReq phone
    void $ withClient env (_requestCode accountClient req)
    runMockApp env $ findAccount phone >>= \case
      Nothing -> liftIO $ expectationFailure "Cannot find account in DB"
      Just Account {..} -> liftIO $ aPhone `shouldBe` phone
  , testCase "`POST /signIn/request` throttling too many requests" $ do
    let req = RequestCodeReq $ Phone "+12345678902"
    void $ withClient env (_requestCode accountClient req)
    withClient env (_requestCode accountClient req)
      `shouldThrow` apiStatusCode tooManyRequests429
  , testCase "`POST /signIn` throws error for unknown phone" $ do
    let
      req = SignInReq
        { si_reqPhone = Phone "+99999999999"
        , si_reqCode  = ConfirmationCode "123456"
        }
    withClient env (_signIn accountClient Nothing req)
      `shouldThrow` apiStatusCode unauthorized401
    assertBool "True" True
  , testCase "`POST /signIn` throws error for wrong code" $ do
    let
      phone = Phone "+12345678903"
      req1  = RequestCodeReq phone
      req2 =
        SignInReq { si_reqPhone = phone, si_reqCode = ConfirmationCode "wrong" }
    void $ withClient env (_requestCode accountClient req1)
    withClient env (_signIn accountClient Nothing req2)
      `shouldThrow` apiStatusCode unauthorized401
  , testCase "`POST /signIn` returns JWT for valid phone and code" $ do
    let
      phone = Phone "+12345678904"
      req1  = RequestCodeReq phone
    void $ withClient env (_requestCode accountClient req1)
    code <- runMockApp env $ findAccount phone >>= findCode >>= \case
      Nothing   -> pure $ ConfirmationCode "wrong"
      Just code -> pure code
    let req2 = SignInReq { si_reqPhone = phone, si_reqCode = code }
    SignInResp {..} <- withClient env (_signIn accountClient Nothing req2)
    si_respToken `shouldBe` JwtToken (getJwtToken si_respToken)
  , testCase "`POST /signIn` creates auth log entry" $ do
    let
      phone = Phone "+12345678905"
      req1  = RequestCodeReq phone
    void $ withClient env (_requestCode accountClient req1)
    account <- runMockApp env $ findAccount phone
    code    <- runMockApp env $ findCode account >>= \case
      Nothing   -> pure $ ConfirmationCode "wrong"
      Just code -> pure code
    let req2 = SignInReq { si_reqPhone = phone, si_reqCode = code }
    void $ withClient env (_signIn accountClient Nothing req2)
    runMockApp env $ findAuthLog account >>= \case
      Nothing -> liftIO $ expectationFailure "Cannot find auth log in DB"
      Just AuthLog {..} -> liftIO $ alCode `shouldBe` code
  ]


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
