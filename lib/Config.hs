{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where

import Dhall

data Config =
  Config {
    port :: Natural,
    database  :: String
  } deriving (Generic, Eq, Show)

instance Interpret Config
