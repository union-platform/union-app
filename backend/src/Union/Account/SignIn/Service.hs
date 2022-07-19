-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | This module represents signIn related service; here is actions with DB or
-- another external services.
module Union.Account.SignIn.Service
  ( sendConfirmationCode
  , signIn
  ) where

import Relude

import qualified Rel8 as Sql

import Data.Time.Clock (UTCTime, getCurrentTime)
import Hasql.Statement (Statement)
import Network.Socket (SockAddr)
import Rel8 ((&&.), (==.), (>.), Expr, Query, Result)
import Rel8.Expr.Time (addTime, now, seconds)

import qualified Core

import Core.Error (throwError)
import Core.Logger (Severity(..), logDebug)
import Core.Sender (ConfirmationCode)

import Union.Account.Schema (Account(..), AccountId, accountSchema)
import Union.Account.SignIn.Schema
  ( AuthLog(..)
  , Confirmation(..)
  , ConfirmationScope(..)
  , authLogSchema
  , confirmationSchema
  )
import Union.Account.SignIn.Types (UserAgent)
import Union.App.Db (executeS, selectExists, selectOne)
import Union.App.Env (WithDb, WithError, WithLog, WithSender)
import Union.App.Error (Error(..))
import Union.Auth (JwtToken, generateJwtToken)


-- | Query to check whether provided code is valid.
checkCode
  :: ConfirmationScope
  -> AccountId
  -> Maybe ConfirmationCode
  -> Query (Confirmation Expr)
checkCode scope accountId code = do
  confirmation@Confirmation {..} <- Sql.each confirmationSchema
  Sql.where_
    $   (cAccountId ==. Sql.lit accountId)
    &&. (cScope ==. Sql.lit scope)
    &&. (cUsed ==. Sql.false)
    &&. (cExpiredAt >. now)
    &&. maybe Sql.true (\c -> cCode ==. Sql.lit c) code
  pure confirmation

-- | Generates OTP and sends it to user phone.
sendConfirmationCode
  :: (WithDb m, WithError m, WithLog m, WithSender m)
  => ConfirmationScope
  -> Account Result
  -> m ()
sendConfirmationCode scope Account { aAccountId, aPhone } = do
  -- check, whether there is not expired active code
  whenJustM (executeS isCodeGenerated) $ \expiredT -> do
    currentT <- liftIO getCurrentTime
    throwError Info . ToManyRequests $ Core.secondsInDiff expiredT currentT
  -- generate new code
  code <- Core.generateCode
  executeS $ insertCode code
  logDebug $ "Generated OTP " <> show code <> " for " <> show aPhone
  -- send code via sms service
  void $ Core.sendCode aPhone code
  where
    isCodeGenerated :: Statement () (Maybe UTCTime)
    isCodeGenerated = selectOne $ do
      Confirmation { cExpiredAt } <- checkCode SignIn aAccountId Nothing
      pure cExpiredAt

    insertCode :: ConfirmationCode -> Statement () ()
    insertCode code = Sql.insert $ Sql.Insert
      { into       = confirmationSchema
      , rows       = Sql.values
        [ Confirmation
            { cAccountId = Sql.lit aAccountId
            , cScope     = Sql.lit scope
            , cCode      = Sql.lit code
            , cCreatedAt = now
            , cExpiredAt = addTime (seconds 60) now
            , cUsed      = Sql.false
            }
        ]
      , onConflict = Sql.Abort
      , returning  = pass
      }

-- | Generates JWT token and records sign in.
signIn
  :: (WithDb m, WithError m, WithLog m)
  => Account Result
  -> ConfirmationCode
  -> SockAddr
  -> Maybe UserAgent
  -> m JwtToken
signIn account code ip agent = do
  -- check, whether there code is valid
  unlessM (executeS isCodeValid) . throwError Info $ NotAllowed
    "Provided phone number or OTP is not valid"
  -- update activation date, if not set
  whenNothing_ (aActivatedAt account) $ executeS updateActivatedAt
  -- generate JWT
  token <- generateJwtToken $ aAccountId account
  logDebug $ "Generated " <> show token <> " for " <> show (aPhone account)
  -- record auth in log
  executeS insertAuthLog
  pure token
  where
    isCodeValid :: Statement () Bool
    isCodeValid =
      selectExists $ checkCode SignIn (aAccountId account) (Just code)

    updateActivatedAt :: Statement () ()
    updateActivatedAt = Sql.update $ Sql.Update
      { target      = accountSchema
      , from        = pass
      , set         = \_ row -> row { aActivatedAt = Sql.nullify now }
      , updateWhere = \_ row -> aAccountId row ==. Sql.lit (aAccountId account)
      , returning   = pass
      }

    insertAuthLog :: Statement () ()
    insertAuthLog = Sql.insert $ Sql.Insert
      { into       = authLogSchema
      , rows       = Sql.values
        [ AuthLog
            { alAccountId = Sql.lit (aAccountId account)
            , alCode      = Sql.lit code
            , alIp        = Sql.lit $ show ip
            , alClient    = Sql.lit agent
            , alCreatedAt = now
            }
        ]
      , onConflict = Sql.Abort
      , returning  = pass
      }
