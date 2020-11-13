{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model where

import Data.Text
import Data.Time.Clock
import Database.Persist.Quasi
import Database.Persist.TH

mkMigrate
  "migrateAll"
  ( $(persistFileWith lowerCaseSettings "config/models/report")
      ++ $(persistFileWith lowerCaseSettings "config/models/user")
      ++ $(persistFileWith lowerCaseSettings "config/models/post")
  )
