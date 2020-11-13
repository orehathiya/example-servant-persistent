{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.User where

import Data.Text
import Database.Persist
import Model.User
import Servant
import Servant.Docs

type UsersApi = UserAdd :<|> UserGet

type UserAdd = "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe UserId)

type UserGet = "user" :> "get" :> Capture "name" Text :> Get '[JSON] (Entity User)

instance ToCapture (Capture "name" Text) where
  toCapture _ = DocCapture "name" "(text) person name"
