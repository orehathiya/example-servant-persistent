{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Server.Post where

import Api.Post
import Api.RestApi
import Control.Monad.IO.Class
import Data.Time
import Database.Persist
import qualified Model.Post as MP
import Servant
import Server
import Util

postsServer :: AppServer PostsApi
postsServer =
  serverFor postsGetH postAddH postGetH postUpdateH postDeleteH

postsGetH :: AppHandler [Entity MP.Post]
postsGetH = runSql $ selectList [] []

postAddH :: MP.Post -> AppHandler (Maybe MP.PostId)
postAddH newPost = do
  time <- liftIO getCurrentTime
  runSql $ Just <$> insert newPost {MP.created = time}

postGetH :: MP.PostId -> AppHandler (Entity MP.Post)
postGetH postid = do
  mPost <- runSql $ get postid
  case mPost of
    Just post -> return $ Entity postid post
    Nothing -> throwError $ err404 {errBody = "(╯°□°）╯︵ ┻━┻)."}

postUpdateH :: MP.PostId -> MP.Post -> AppHandler NoContent
postUpdateH postid newPost =
  runSql $ do
    replace postid newPost
    return NoContent

postDeleteH :: MP.PostId -> AppHandler NoContent
postDeleteH postid =
  runSql $ do
    delete postid
    return NoContent
