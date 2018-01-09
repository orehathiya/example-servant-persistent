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
import qualified Model.Post as MP

postsServer :: ConnectionPool -> Server PostsApi
postsServer pool = postAddH pool :<|> postGetH pool :<|> postsGetH pool

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

postsGetH :: ConnectionPool ->  Handler [Entity MP.Post]
postsGetH pool = liftIO $ postsGet pool
  where
    postsGet :: ConnectionPool -> IO [Entity MP.Post]
    postsGet pool = flip runSqlPersistMPool pool $ selectList [] []
