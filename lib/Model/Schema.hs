{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Schema where

import Control.Lens ((&), (?~), (.~), mapped)
import Data.Proxy
import Data.Swagger
import Data.Swagger.Declare
import Data.Text
import Data.Typeable (Typeable, typeRep, showsTypeRep)
import Database.Persist

defaultKeyDeclareNamedSchema :: p -> Declare (Definitions Schema) NamedSchema
defaultKeyDeclareNamedSchema _ =
  declareNamedSchema (Proxy @Int) & mapped . schema . example ?~ "1"

defaultKeyToParamSchema :: p -> ParamSchema t
defaultKeyToParamSchema _ = mempty & type_ ?~ SwaggerInteger

defaultEntityDeclareNamedSchema
  :: forall record (proxy :: * -> *).
     (Typeable record, ToSchema (Key record), ToSchema record)
  => proxy (Entity record) -> Declare (Definitions Schema) NamedSchema
defaultEntityDeclareNamedSchema _ = do
  let valProxy = Proxy @record
      name_ = showsTypeRep (typeRep valProxy) "Entity"
  keySchema <- declareSchemaRef (Proxy @(Key record))
  valSchema <- declareSchemaRef valProxy
  return $ NamedSchema (Just $ pack name_) $ mempty & type_ ?~ SwaggerObject &
    properties .~
    [("entityKey", keySchema), ("entityVal", valSchema)]
