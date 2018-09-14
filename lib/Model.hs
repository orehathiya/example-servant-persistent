{-# LANGUAGE TemplateHaskell #-}

module Model where

import Data.Time.Clock
import Data.Text
import Database.Persist.TH
import Database.Persist.Quasi

mkMigrate
  "migrateAll"
  ($(persistFileWith lowerCaseSettings "config/models/report") ++
   $(persistFileWith lowerCaseSettings "config/models/user") ++
   $(persistFileWith lowerCaseSettings "config/models/post"))
