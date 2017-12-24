{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Yaml.Config
import Prelude hiding (readFile)
import Network.Wai.Handler.Warp as Warp

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
  app <- mkApp $ database config
  Warp.run (port config) app
