{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module App where

import Control.Monad.Logger (runStderrLoggingT)
import Data.String.Conversions
import Database.Persist.Sql
import Database.Persist.Sqlite
import Network.Wai
import Servant

import Api
import Model
import Server.Report
import Server.User

server :: ConnectionPool -> Server Api
server pool = (usersServer pool :<|> reportsServer pool) :<|> serveDirectoryFileServer "client/static"

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ app pool
