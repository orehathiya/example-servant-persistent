{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Post where

import Database.Persist
import Servant
import Servant.Docs

import qualified Model.Post as MP

type PostsApi = PostAdd :<|> PostGet :<|> PostsGet

type PostAdd = "posts" :> ReqBody '[JSON] MP.Post :> Post '[JSON] (Maybe MP.PostId)

type PostGet = "posts" :> Capture "postid" MP.PostId :> Get '[JSON] (Entity MP.Post)

instance ToCapture (Capture "postid" MP.PostId) where
  toCapture _ = DocCapture "postid" "(int) post id"

type PostsGet = "posts" :> Get '[JSON] [Entity MP.Post]
