{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.User where

import Data.Text
import Database.Persist
import Servant

import Api.User
import Model.User
import Server
import Util

usersServer :: AppServer UsersApi
usersServer = userAddH :<|> userGetH

userAddH :: User -> AppHandler (Maybe UserId)
userAddH newUser =
  runSql $ do
    exists <- selectFirst [Name ==. name newUser] []
    case exists of
      Nothing -> Just <$> insert newUser
      Just _ -> return Nothing

userGetH :: Text -> AppHandler (Entity User)
userGetH name = do
  mUser <- runSql $ selectFirst [Name ==. name] []
  case mUser of
    Just user -> return user
    Nothing -> throwError $ err404 {errBody = "(╯°□°）╯︵ ┻━┻)."}
