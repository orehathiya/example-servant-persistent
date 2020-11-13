{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Post where

import Api.RestApi
import qualified Model.Post as MP
import Servant
import Servant.Docs

type PostsApi = "posts" :> APIFor MP.Post MP.PostId

instance ToCapture (Capture "id" MP.PostId) where
  toCapture _ = DocCapture "id" "(int) post id"
