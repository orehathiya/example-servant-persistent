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

module Model.Post where

import Data.Swagger hiding (name, title)
import Data.Text
import Data.Time.Calendar
import Data.Time.Clock
import Database.Persist.Quasi
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics
import Model ()
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

instance ToSample (Entity Post) where
  toSamples _ =
    samples
      [ Entity (PostKey 1) (Post "title1" "body1" dummyTime)
      , Entity (PostKey 2) (Post "title2" "body2" dummyTime)
      , Entity (PostKey 3) (Post "title3" "body3" dummyTime)
      ]

instance ToSample Post where
  toSamples _ =
    samples
      [ Post "title1" "body1" dummyTime
      , Post "title2" "body2" dummyTime
      , Post "title3" "body3" dummyTime
      ]

instance ToSample PostId where
  toSamples _ = singleSample (PostKey 1)

instance ToSchema Post

dummyTime :: UTCTime
dummyTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
