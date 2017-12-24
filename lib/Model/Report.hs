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

module Model.Report where

import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Quasi
import GHC.Generics
import Servant.Docs

mkPersist sqlSettings {
  mpsPrefixFields = False,
  mpsEntityJSON = Just EntityJSON { entityToJSON = 'keyValueEntityToJSON, entityFromJSON = 'keyValueEntityFromJSON}
  }
  $(persistFileWith lowerCaseSettings "config/models/report")

instance ToSample (Entity Report) where
  toSamples _ =
    samples
      [ Entity (ReportKey 1) (Report 42 3 1)
      , Entity (ReportKey 1) (Report 32 1 1)
      ]

instance ToSample Report where
  toSamples _ = samples [Report 42 3 1, Report 32 1 1]

instance ToSample ReportId where
  toSamples _ = singleSample (ReportKey 1)
