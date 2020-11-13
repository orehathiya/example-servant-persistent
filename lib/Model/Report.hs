{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.Report where

import Data.Swagger
import Database.Persist.Quasi
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Servant.Docs

import Model.Schema
       (defaultKeyDeclareNamedSchema, defaultEntityDeclareNamedSchema,
        defaultKeyToParamSchema)

mkPersist
  sqlSettings
  { mpsPrefixFields = False
  , mpsEntityJSON =
      Just
        EntityJSON
        { entityToJSON = 'keyValueEntityToJSON
        , entityFromJSON = 'keyValueEntityFromJSON
        }
  }
  $(persistFileWith lowerCaseSettings "config/models/report")

instance ToSample Report where
  toSamples _ = samples [Report 42 3 1, Report 32 1 1]

instance ToSchema Report

instance ToSample ReportId where
  toSamples _ = singleSample (ReportKey 1)

instance ToSchema ReportId where
  declareNamedSchema = defaultKeyDeclareNamedSchema

instance ToParamSchema ReportId where
  toParamSchema = defaultKeyToParamSchema

instance ToSample (Entity Report) where
  toSamples _ =
    samples
      [ Entity (ReportKey 1) (Report 42 3 1)
      , Entity (ReportKey 1) (Report 32 1 1)
      ]

instance ToSchema (Entity Report) where
  declareNamedSchema = defaultEntityDeclareNamedSchema
