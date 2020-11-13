{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Json where

import Data.Aeson
import Data.Aeson.Types
import Database.Persist.Sql

myKeyValueEntityToJSON ::
  ( PersistEntity record,
    ToJSON record,
    ToJSON (Key record),
    ToBackendKey SqlBackend record
  ) =>
  Entity record ->
  Value
myKeyValueEntityToJSON (Entity key value) =
  object
    [ "entityKey" .= fromSqlKey key,
      "entityVal" .= value
    ]

myKeyValueEntityFromJSON ::
  ( PersistEntity record,
    FromJSON record,
    FromJSON (Key record),
    ToBackendKey SqlBackend record
  ) =>
  Value ->
  Parser (Entity record)
myKeyValueEntityFromJSON (Object o) =
  Entity
    <$> fmap toSqlKey (o .: "entityKey")
    <*> o .: "entityVal"
myKeyValueEntityFromJSON _ = fail "keyValueEntityFromJSON: not an object"
