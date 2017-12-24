{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.User where

import Control.Monad.IO.Class
import Data.Text
import Database.Persist
import Database.Persist.Sql
import Servant

import Api.User
import Model.User

usersServer :: ConnectionPool -> Server UsersApi
usersServer pool = userAddH pool :<|> userGetH pool

userAddH :: ConnectionPool -> User -> Handler (Maybe UserId)
userAddH pool newUser = liftIO $ userAdd pool newUser
  where
    userAdd :: ConnectionPool -> User -> IO (Maybe UserId)
    userAdd pool newUser =
      flip runSqlPersistMPool pool $ do
        exists <- selectFirst [Name ==. name newUser] []
        case exists of
          Nothing -> Just <$> insert newUser
          Just _ -> return Nothing

userGetH :: ConnectionPool -> Text -> Handler (Entity User)
userGetH pool name = do
  mUser <- userGet pool name
  case mUser of
    Just user -> return user
    Nothing -> throwError $ err404 {errBody = "(╯°□°）╯︵ ┻━┻)."}
  where
    userGet :: ConnectionPool -> Text -> Handler (Maybe (Entity User))
    userGet pool name =
      liftIO $ flip runSqlPersistMPool pool $ selectFirst [Name ==. name] []
