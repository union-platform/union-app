-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: MPL-2.0

-- | Roundtrip tests for JWT.
module Test.Jwt
  ( jwtTests
  ) where

import Relude

import qualified Web.JWT as Jwt

import Control.Monad.Morph (hoist)
import Hedgehog ((===), Gen, Property, forAll, property, tripping)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Core.Jwt
  ( JwtPayload(..)
  , MonadJwt(..)
  , decodeIntIdPayload
  , decodeTextIdPayload
  , encodeIntIdPayload
  , encodeTextIdPayload
  )
import Core.Time (Seconds(..))

import Test.Gen (genInt, genSeconds, genText)
import Test.Mock (MockEnv, runMockApp)


jwtTests :: MockEnv -> TestTree
jwtTests env = testGroup
  "JWT roundtrip properties"
  [ testProperty "fromJwtMap . toJwtMap @Int  ≡ Just"
    $ jwtRoundtrip genInt encodeIntIdPayload decodeIntIdPayload
  , testProperty "fromJwtMap . toJwtMap @Text ≡ Just"
    $ jwtRoundtrip genText encodeTextIdPayload decodeTextIdPayload
  , testProperty "verifyJwt  . createJwt      ≡ True" $ createAndVerifyJwt env
  ]

jwtRoundtrip
  :: (Eq a, Show a)
  => Gen a
    -- ^ Payload generator
  -> (JwtPayload a -> Jwt.ClaimsMap)
    -- ^ Encoder
  -> (Jwt.ClaimsMap -> Maybe (JwtPayload a))
    -- ^ Decoder
  -> Property
jwtRoundtrip gen encode decode = property $ do
  randomPayload <- JwtPayload <$> forAll gen
  tripping randomPayload encode decode

createAndVerifyJwt :: MockEnv -> Property
createAndVerifyJwt env = property . hoist (runMockApp env) $ do
  seconds         <- forAll genSeconds
  payload         <- forAll genPayload
  verifiedPayload <- lift $ makeAndVerifyToken seconds payload
  verifiedPayload === Just payload

genPayload :: Gen (JwtPayload Int)
genPayload = JwtPayload <$> genInt

makeAndVerifyToken
  :: MonadJwt Int m => Seconds -> JwtPayload Int -> m (Maybe (JwtPayload Int))
makeAndVerifyToken expiry = mkJwtToken expiry >=> verifyJwtToken
