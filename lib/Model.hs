{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model where

import Control.Lens ((&), (?~), (.~), mapped)
import Data.Aeson (ToJSON)
import Data.Proxy
import Data.Swagger
import Data.Text
import Data.Time.Clock
import Data.Typeable (Typeable, typeRep, showsTypeRep)
import Database.Persist
import Database.Persist.Quasi
import Database.Persist.TH

mkMigrate
  "migrateAll"
  ($(persistFileWith lowerCaseSettings "config/models/report") ++
   $(persistFileWith lowerCaseSettings "config/models/user") ++
   $(persistFileWith lowerCaseSettings "config/models/post"))

instance ToSchema (Key a) where
  declareNamedSchema _ =
    declareNamedSchema (Proxy @Int) & mapped . schema . type_ .~ SwaggerInteger &
    mapped .
    schema .
    example ?~
    "1"

instance ToParamSchema (Key a) where
  toParamSchema _ = mempty & type_ .~ SwaggerInteger

instance (ToJSON (Entity a), ToSchema (Key a), ToSchema a, Typeable a) =>
         ToSchema (Entity a) where
  declareNamedSchema _ = do
    let valProxy = Proxy @a
        name_ = showsTypeRep (typeRep valProxy) "Entity"
    keySchema <- declareSchemaRef (Proxy @(Key a))
    valSchema <- declareSchemaRef valProxy
    return $ NamedSchema (Just $ pack name_) $ mempty & type_ .~ SwaggerObject &
      properties .~
      [("entityKey", keySchema), ("entityVal", valSchema)]
