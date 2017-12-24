{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import Data.Text hiding (concat)
import Database.Persist.TH
import Database.Persist.Quasi

mkMigrate "migrateAll" (
  $(persistFileWith lowerCaseSettings "config/models/report") ++
  $(persistFileWith lowerCaseSettings "config/models/user")
  )
