-- SPDX-FileCopyrightText: 2021-2022 Union
--
-- SPDX-License-Identifier: MPL-2.0

{- HLINT ignore "Use ?~" -}

-- | Helper functions to write @swagger@ instances easier.
module Core.Swagger
  ( WithSwagger
  , schemaRef
  , namedSchema
  , declareSingleFieldSchema
  , genericNamedSchema
  ) where

import Relude
import Relude.Extra.Lens ((.~))
import Relude.Extra.Type (typeName)

import Data.Foldable (foldl)
import Data.OpenApi
  ( Definitions
  , HasType(type_)
  , NamedSchema(NamedSchema)
  , OpenApiType(OpenApiObject)
  , Referenced
  , Schema
  , ToSchema
  , declareSchemaRef
  , fromAesonOptions
  , genericDeclareNamedSchema
  , properties
  , required
  )
import Data.OpenApi.Declare (Declare, DeclareT)
import Data.OpenApi.Internal.Schema (GToSchema)
import Deriving.Aeson (AesonOptions(aesonOptions))
import GHC.Generics (Rep)
import Servant.API (type (:<|>))
import Servant.Swagger.UI (SwaggerSchemaUI)


-- | Attach a swagger UI to the given @api@.
type WithSwagger url api = api :<|> SwaggerSchemaUI url "swagger.json"

-- | Shorter version of 'declareSchemaRef'. So instead of
-- @
-- 'declareSchemaRef' @MyType 'Proxy'
-- @
-- you can write
-- @
-- 'schemaRef' @MyType
-- @
schemaRef
  :: forall t . ToSchema t => Declare (Definitions Schema) (Referenced Schema)
schemaRef = declareSchemaRef @t Proxy

-- | Helper function to return named schemas. So instead of:
-- @
-- pure $ 'NamedSchema' (Just \"LoginResponse\") $ 'mempty'
--   & type_ .~ OpenApiObject
--   & properties .~ fromList
--       [("jwtToken", jwtTokenSchema)]
--   & required .~ ["jwtToken"]
-- @
-- you will use it like:
-- @
-- 'namedSchema' @LoginResponse $ \s -> s
--   & properties .~ fromList
--       [("jwtToken", jwtTokenSchema)]
--   & required .~ ["jwtToken"]
-- @
namedSchema
  :: forall t f
   . (Typeable t, Applicative f)
  => (Schema -> Schema)
  -> f NamedSchema
namedSchema updateSchema = pure $ NamedSchema
  (Just $ typeName @t)
  (updateSchema $ mempty & type_ .~ Just OpenApiObject)

-- | Helper function to declare 'ToSchema' instances for data types that contain
-- only single field.
declareSingleFieldSchema
  :: forall field a proxy
   . (ToSchema field, Typeable a)
  => Text
  -> proxy a
  -> Declare (Definitions Schema) NamedSchema
declareSingleFieldSchema fieldName _ = do
  fieldsSchema <- schemaRef @field
  namedSchema @a $ \s ->
    s
      &  properties
      .~ fromList [(fieldName, fieldsSchema)]
      &  required
      .~ [fieldName]

-- | Helper function to declare 'NamedSchema' using derived JSON instances by
-- @Core.Json@ module.
genericNamedSchema
  :: forall j t
   . (AesonOptions j, Generic t, Typeable t, GToSchema (Rep t))
  => [  DeclareT (Definitions Schema) Identity NamedSchema
     -> DeclareT (Definitions Schema) Identity NamedSchema
     ]
  -> Proxy t
  -> Declare (Definitions Schema) NamedSchema
genericNamedSchema extra proxy = foldl (&) schema extra
  where
    schema =
      genericDeclareNamedSchema (fromAesonOptions $ aesonOptions @j) proxy
