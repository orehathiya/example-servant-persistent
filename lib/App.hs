{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module App where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.String.Conversions
import Database.Persist.Sql
import Database.Persist.Sqlite
import Network.Wai
import Servant

import Api
import Model
import Server
import Server.Report
import Server.User
import Server.Post
import Config
import Environment

app :: Environment -> Application
app env = serve api $ appToServer env

nt :: Environment -> AppHandler a -> Handler a
nt env r = runReaderT r env

appToServer :: Environment -> Server Api
appToServer env = hoistServer api (nt env) server

server :: AppServer Api
server = usersServer :<|> reportsServer :<|> postsServer

mkApp :: Config -> IO Application
mkApp cfg = do
  pool <- runStderrLoggingT $ createSqlitePool (cs $ database cfg) 5
  runSqlPool (runMigration migrateAll) pool
  return $ app $ Environment pool cfg
