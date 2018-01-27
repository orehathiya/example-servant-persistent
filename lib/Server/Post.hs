{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Post where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Database.Persist
import Database.Persist.Sql
import Data.Time
import Servant

import Api.Post
import Api.RestApi
import qualified Model.Post as MP
import Environment
import Server

postsServer :: AppServer PostsApi
postsServer =
  serverFor postsGetH postAddH postGetH postUpdateH postDeleteH

postsGetH :: AppHandler [Entity MP.Post]
postsGetH = do
  pool <- asks pool
  liftIO $ postsGet pool
  where
    postsGet :: ConnectionPool -> IO [Entity MP.Post]
    postsGet pool = flip runSqlPersistMPool pool $ selectList [] []

postAddH :: MP.Post -> AppHandler (Maybe MP.PostId)
postAddH newPost = do
  pool <- asks pool
  liftIO $ postAdd pool newPost
  where
    postAdd :: ConnectionPool -> MP.Post -> IO (Maybe MP.PostId)
    postAdd pool newPost = do
      time <- getCurrentTime
      flip runSqlPersistMPool pool $ Just <$> insert newPost {MP.created = time}

postGetH :: MP.PostId -> AppHandler (Entity MP.Post)
postGetH postid = do
  pool <- asks pool
  mPost <- postGet pool postid
  case mPost of
    Just post -> return $ Entity postid post
    Nothing -> throwError $ err404 {errBody = "(╯°□°）╯︵ ┻━┻)."}
  where
    postGet :: ConnectionPool -> MP.PostId -> AppHandler (Maybe MP.Post)
    postGet pool postid = liftIO $ flip runSqlPersistMPool pool $ get postid

postUpdateH :: MP.PostId -> MP.Post -> AppHandler NoContent
postUpdateH postid newPost = do
  pool <- asks pool
  liftIO $ postUpdate pool postid newPost
  where
    postUpdate :: ConnectionPool -> MP.PostId -> MP.Post -> IO NoContent
    postUpdate pool postid newPost = do
      flip runSqlPersistMPool pool $ replace postid newPost
      return NoContent

postDeleteH :: MP.PostId -> AppHandler NoContent
postDeleteH postid = do
  pool <- asks pool
  liftIO $ postDelete pool postid
  where
    postDelete :: ConnectionPool -> MP.PostId -> IO NoContent
    postDelete pool postid = do
      flip runSqlPersistMPool pool $ delete postid
      return NoContent
