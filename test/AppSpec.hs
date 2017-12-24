{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import Data.Text
import Data.Maybe
import Database.Persist
import Network.HTTP.Client hiding (port)
import Network.HTTP.Media.MediaType
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp
import Servant.API hiding (addHeader)
import Servant.Client
import Servant.Common.Req hiding (manager, baseUrl)
import Test.Hspec
import Test.Mockery.Directory

import Api
import App hiding (app)
import Model.User
import Model.Report

userAdd :: User -> ClientM (Maybe (Key User))
userGet :: Text -> ClientM (Entity User)
reportGet :: ClientM [Entity Report]
(userAdd :<|> userGet) :<|> reportGet = client appApi

spec :: Spec
spec =
  around withApp $ do
    describe "/user/get" $
      it "throw Exception for non-existing users" $ \port ->
        try port (userGet "foo") `shouldReturn`
        Left
          (FailureResponse
             (UrlReq (BaseUrl Http "localhost" port "") defReq)
             notFound404
             ("application" // "octet-stream")
             "(╯°□°）╯︵ ┻━┻).")
    describe "/user/add" $ do
      it "allows to add a user" $ \port -> do
        let user :: User = User "Alice" 1
        res <- try port $ userAdd user
        let userId :: UserId = case res of
              Right mUserId -> fromMaybe (UserKey 0) mUserId
              Left _ -> UserKey 0
        try port (userGet "Alice") `shouldReturn`Right (Entity userId user)
      it "allows to add two users" $ \port -> do
        let a = User "Alice" 1
        let b = User "Bob" 2
        _ <- try port $ userAdd a
        res <- try port $ userAdd b
        let userId :: UserId = case res of
              Right mUserId -> fromMaybe (UserKey 0) mUserId
              Left _ -> UserKey 0
        try port (userGet "Bob") `shouldReturn` Right (Entity userId b)
      it "returns Nothing when adding the same user twice" $ \port -> do
        let a = User "Alice" 1
        _ <- try port $ userAdd a
        try port (userAdd a) `shouldReturn` Right Nothing

withApp :: (Int -> IO a) -> IO a
withApp action =
  inTempDirectory $ do
    app <- mkApp "sqlite.db"
    testWithApplication (return app) action

try :: Int -> ClientM a -> IO (Either ServantError a)
try port action = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  runClientM action $ ClientEnv manager baseUrl
