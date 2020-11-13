{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Api.Post
import Api.Report
import Api.User
import Data.Proxy
import Servant.API

type Api = UsersApi :<|> ReportsApi :<|> PostsApi

api :: Proxy Api
api = Proxy
