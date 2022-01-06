-- SPDX-FileCopyrightText: 2021 Union
--
-- SPDX-License-Identifier: MPL-2.0

-- | Roundtrip tests for JWT.
module Test.Jwt
  ( jwtTests
  ) where

import Relude

import qualified Web.JWT as Jwt

import Hedgehog (Gen, Group(..), Property, forAll, property, tripping, (===))

import Core.Jwt
  (JwtPayload(..), MonadJwt(..), decodeIntIdPayload, decodeTextIdPayload,
  encodeIntIdPayload, encodeTextIdPayload)
import Core.Time (Seconds(..))

import Test.Gen (genInt, genSeconds, genText)
import Test.Mock (runMockApp)


jwtTests :: Group
jwtTests = Group "JWT roundtrip properties"
  [ "fromJwtMap . toJwtMap @Int  ≡ Just" `named`
      jwtRoundtrip genInt encodeIntIdPayload decodeIntIdPayload
  , "fromJwtMap . toJwtMap @Text ≡ Just" `named`
      jwtRoundtrip genText encodeTextIdPayload decodeTextIdPayload
  , "verifyJwt  . createJwt      ≡ True" `named` createAndVerifyJwt
  ]
  where
    named :: a -> b -> (a, b)
    named = (,)

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

createAndVerifyJwt :: Property
createAndVerifyJwt = property $ do
    seconds <- forAll genSeconds
    payload <- forAll genPayload
    verifiedPayload <- liftIO $ runMockApp $ makeAndVerifyToken seconds payload
    verifiedPayload === Just payload


genPayload :: Gen (JwtPayload Int)
genPayload = JwtPayload <$> genInt

makeAndVerifyToken
    :: MonadJwt Int m
    => Seconds
    -> JwtPayload Int
    -> m (Maybe (JwtPayload Int))
makeAndVerifyToken expiry = mkJwtToken expiry >=> verifyJwtToken
