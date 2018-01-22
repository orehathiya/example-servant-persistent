{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Post where

import Control.Monad.IO.Class
import Database.Persist
import Database.Persist.Sql
import Data.Time
import Servant

import Api.Post
import Api.RestApi
import qualified Model.Post as MP

postsServer :: ConnectionPool -> Server PostsApi
postsServer pool = serverFor pool postsGetH postAddH postGetH postUpdateH postDeleteH

postsGetH :: ConnectionPool ->  Handler [Entity MP.Post]
postsGetH pool = liftIO $ postsGet pool
  where
    postsGet :: ConnectionPool -> IO [Entity MP.Post]
    postsGet pool = flip runSqlPersistMPool pool $ selectList [] []

postAddH :: ConnectionPool -> MP.Post -> Handler (Maybe MP.PostId)
postAddH pool newPost = liftIO $ postAdd pool newPost
  where
    postAdd :: ConnectionPool -> MP.Post -> IO (Maybe MP.PostId)
    postAdd pool newPost = do
      time <- getCurrentTime
      flip runSqlPersistMPool pool $ Just <$> insert newPost {MP.created = time}

postGetH :: ConnectionPool -> MP.PostId -> Handler (Entity MP.Post)
postGetH pool postid = do
  mPost <- postGet pool postid
  case mPost of
    Just post -> return $ Entity postid post
    Nothing -> throwError $ err404 {errBody = "(╯°□°）╯︵ ┻━┻)."}
  where
    postGet :: ConnectionPool -> MP.PostId -> Handler (Maybe MP.Post)
    postGet pool postid = liftIO $ flip runSqlPersistMPool pool $ get postid

postUpdateH :: ConnectionPool -> MP.PostId -> MP.Post -> Handler NoContent
postUpdateH pool postid newPost = liftIO $ postUpdate pool postid newPost
  where
    postUpdate :: ConnectionPool -> MP.PostId -> MP.Post -> IO NoContent
    postUpdate pool postid newPost = do
      flip runSqlPersistMPool pool $ replace postid newPost
      return NoContent

postDeleteH :: ConnectionPool -> MP.PostId -> Handler NoContent
postDeleteH pool postid = liftIO $ postDelete pool postid
  where
    postDelete :: ConnectionPool -> MP.PostId -> IO NoContent
    postDelete pool postid = do
      flip runSqlPersistMPool pool $ delete postid
      return NoContent
