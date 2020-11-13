{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import App hiding (app)
import Config
import Dhall
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors (cors, corsMethods, corsRequestHeaders, simpleCorsResourcePolicy)
import Prelude

main :: IO ()
main = do
  config :: Config <- input auto "./config/config.dhall"
  Main.run config

run :: Config -> IO ()
run config = do
  app <- mkApp config
  Warp.run (fromIntegral $ port config) $ cors (const $ Just policy) app
  where
    policy =
      simpleCorsResourcePolicy
        { corsMethods = [methodGet, methodPost, methodHead, methodPut, methodDelete],
          corsRequestHeaders = [hContentType]
        }
