{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Post where

import Servant
import Servant.Docs

import Api.RestApi

import qualified Model.Post as MP

type PostsApi = "posts" :> APIFor MP.Post MP.PostId

instance ToCapture (Capture "id" MP.PostId) where
  toCapture _ = DocCapture "id" "(int) post id"
