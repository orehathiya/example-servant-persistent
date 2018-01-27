{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Yaml.Config
import Prelude hiding (readFile)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, corsMethods, corsRequestHeaders)
import Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header

import App hiding (app)
import Config

main :: IO ()
main = do
  mconfig :: Maybe Config <- loadYamlSettings ["config/settings.yml"] [] useEnv
  case mconfig of
    Just config -> Main.run config
    Nothing -> print ("failed to read config file" :: String)

run :: Config -> IO ()
run config = do
  app <- mkApp config
  Warp.run (port config) $ cors (const $ Just policy) app
  where
    policy = simpleCorsResourcePolicy
               { corsMethods = [methodGet, methodPost, methodHead, methodPut, methodDelete]
               , corsRequestHeaders = [ hContentType ] }
