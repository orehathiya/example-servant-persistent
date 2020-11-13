{-# LANGUAGE StandaloneDeriving #-}
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
{-# LANGUAGE UndecidableInstances #-}

module Model.Post where

import Data.Swagger hiding (name, title)
import Data.Text
import Data.Time.Calendar
import Data.Time.Clock
import Database.Persist.Quasi
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Model.Schema
       (defaultKeyDeclareNamedSchema, defaultEntityDeclareNamedSchema,
        defaultKeyToParamSchema)
import Model.Json
import Servant.Docs

mkPersist
  sqlSettings
  { mpsPrefixFields = False
  , mpsEntityJSON =
      Just
        EntityJSON
        { entityToJSON = 'myKeyValueEntityToJSON
        , entityFromJSON = 'myKeyValueEntityFromJSON
        }
  }
  $(persistFileWith lowerCaseSettings "config/models/post")

instance ToSample Post where
  toSamples _ =
    samples
      [ Post "title1" "body1" dummyTime
      , Post "title2" "body2" dummyTime
      , Post "title3" "body3" dummyTime
      ]

instance ToSchema Post

instance ToSample PostId where
  toSamples _ = singleSample (PostKey 1)

instance ToSchema PostId where
  declareNamedSchema = defaultKeyDeclareNamedSchema

instance ToParamSchema PostId where
  toParamSchema = defaultKeyToParamSchema

instance ToSample (Entity Post) where
  toSamples _ =
    samples
      [ Entity (PostKey 1) (Post "title1" "body1" dummyTime)
      , Entity (PostKey 2) (Post "title2" "body2" dummyTime)
      , Entity (PostKey 3) (Post "title3" "body3" dummyTime)
      ]

instance ToSchema (Entity Post) where
  declareNamedSchema = defaultEntityDeclareNamedSchema

dummyTime :: UTCTime
dummyTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
