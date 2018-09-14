{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Dhall
import Prelude
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsMethods, corsRequestHeaders)
import Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header

import App hiding (app)
import Config

main :: IO ()
main = do
  config :: Config <- input auto "./config/config.dhall"
  Main.run config

run :: Config -> IO ()
run config = do
  app <- mkApp config
  Warp.run (fromIntegral $ port config) $ cors (const $ Just policy) app
  where
    policy = simpleCorsResourcePolicy
               { corsMethods = [methodGet, methodPost, methodHead, methodPut, methodDelete]
               , corsRequestHeaders = [ hContentType ] }
