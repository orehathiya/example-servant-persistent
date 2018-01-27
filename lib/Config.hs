{-# LANGUAGE OverloadedStrings #-}

module Config where

import Data.Aeson
import qualified Data.Yaml as Y

data Config =
  Config {
    port :: Int,
    database  :: String
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .: "port" <*>
    v .: "database"
  parseJSON _ = fail "Expected Object for Config value"
