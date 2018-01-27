module Environment where

import Config
import Database.Persist.Sql

data Environment = Environment
  { pool :: ConnectionPool
  , config :: Config
  } deriving (Show)
