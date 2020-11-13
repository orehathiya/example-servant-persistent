{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module App where

import Api
import Config
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.String.Conversions
import Database.Persist.Sql
import Database.Persist.Sqlite
import Environment
import Model
import Network.Wai
import Servant
import Server
import Server.Post
import Server.Report
import Server.User

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
