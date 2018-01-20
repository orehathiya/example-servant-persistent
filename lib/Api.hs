{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Proxy
import Servant.API

import Api.Report
import Api.User
import Api.Post

type Api = UsersApi :<|> ReportsApi :<|> PostsApi

api :: Proxy Api
api = Proxy
