{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.User where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Text
import Database.Persist
import Database.Persist.Sql
import Servant

import Api.User
import Model.User
import Environment
import Server

usersServer :: AppServer UsersApi
usersServer = userAddH :<|> userGetH

userAddH :: User -> AppHandler (Maybe UserId)
userAddH newUser = do
  pool <- asks pool
  liftIO $ userAdd pool newUser
  where
    userAdd :: ConnectionPool -> User -> IO (Maybe UserId)
    userAdd pool newUser =
      flip runSqlPersistMPool pool $ do
        exists <- selectFirst [Name ==. name newUser] []
        case exists of
          Nothing -> Just <$> insert newUser
          Just _ -> return Nothing

userGetH :: Text -> AppHandler (Entity User)
userGetH name = do
  pool <- asks pool
  mUser <- userGet pool name
  case mUser of
    Just user -> return user
    Nothing -> throwError $ err404 {errBody = "(╯°□°）╯︵ ┻━┻)."}
  where
    userGet :: ConnectionPool -> Text -> AppHandler (Maybe (Entity User))
    userGet pool name =
      liftIO $ flip runSqlPersistMPool pool $ selectFirst [Name ==. name] []
