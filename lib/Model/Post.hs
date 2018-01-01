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

module Model.Post where

import Data.Time.Clock
import Data.Text
import Database.Persist.Quasi
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Servant.Docs
import Data.Time.Calendar

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
  $(persistFileWith lowerCaseSettings "config/models/post")

instance ToSample (Entity Post) where
  toSamples _ =
    samples
      [ Entity
          (PostKey 1)
          (Post
             "title1"
             "body1"
             (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0))
             (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)))
      , Entity
          (PostKey 2)
          (Post
             "title2"
             "body2"
             (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0))
             (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)))
      , Entity
          (PostKey 3)
          (Post
             "title3"
             "body3"
             (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0))
             (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)))
      ]

instance ToSample Post where
  toSamples _ =
    samples
      [ Post
          "title1"
          "body1"
          (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0))
          (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0))
      , Post
          "title2"
          "body2"
          (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0))
          (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0))
      , Post
          "title3"
          "body3"
          (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0))
          (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0))
      ]

instance ToSample PostId where
  toSamples _ = singleSample (PostKey 1)
