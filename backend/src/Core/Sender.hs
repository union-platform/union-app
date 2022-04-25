-- SPDX-FileCopyrightText: 2022 Union
--
-- SPDX-License-Identifier: MPL-2.0

-- | This module provides some tooling to implement sender service (like OTP).
module Core.Sender
  ( -- * Credentials
    Phone(..)
  , SenderAccount(..)
  , AuthToken(..)

    -- * Sender Effect
  , ConfirmationCode(..)
  , MonadSender(..)
  ) where

import Relude

import Core.Random (mkRandomDigits)
import Data.Aeson (FromJSON, ToJSON)
import Rel8 (DBEq, DBType)


-- | Phone number. We assume that number is stored in international format with
-- plus sign.
newtype Phone = Phone { getPhone :: Text }
  deriving stock Generic
  deriving newtype (Show, Eq, ToJSON, FromJSON, DBType, DBEq)

-- | Represents sender service Account.
newtype SenderAccount = SenderAccount { unSenderAccount :: Text }
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

-- | Represents sender server Auth Token.
newtype AuthToken = AuthToken { unAuthToken :: Text }
  deriving stock (Show, Generic)
  deriving newtype (Eq, FromJSON, ToJSON)

-- | Confirmation code.
newtype ConfirmationCode = ConfirmationCode { getCode :: Text }
  deriving stock (Show, Generic)
  deriving newtype (Eq, ToJSON, FromJSON, DBType, DBEq)


-- | This monad represents effect to send something to user.
class MonadIO m => MonadSender m where
  generateCode :: m ConfirmationCode
  generateCode = ConfirmationCode <$> mkRandomDigits 6

  sendCode
    :: HasCallStack
    => Phone
    -- ^ Target phone number.
    -> ConfirmationCode
    -- ^ Code we want to send.
    -> m ()
