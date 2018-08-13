{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import Data.Text
import Data.Maybe
import Database.Persist
import Network.HTTP.Client hiding (port)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Client
import Test.Hspec
import Test.Mockery.Directory

import Api
import App hiding (app)
import Config hiding (port)
import Model.User
import Model.Report
import Model.Post as MP

postClient :: ClientM [Entity MP.Post] :<|> ((MP.Post -> ClientM (Maybe PostId)) :<|> (Key MP.Post -> ClientM (Entity MP.Post) :<|> ((MP.Post -> ClientM NoContent) :<|> ClientM NoContent)))
reportClient :: ClientM [Entity Report]
userClient :: (User -> ClientM (Maybe UserId)) :<|> (Text -> ClientM (Entity User))
userClient :<|> reportClient :<|> postClient = client api

userAdd :: User -> ClientM (Maybe (Key User))
userGet :: Text -> ClientM (Entity User)
userAdd :<|> userGet = userClient

reportGet :: ClientM [Entity Report]
reportGet = reportClient

spec :: Spec
spec =
  around withApp $ do
    describe "/user/get" $
      it "throw Exception for non-existing users" $ \port -> do
        Left (FailureResponse (Response rstatus _ rversion rbody)) <- try port (userGet "foo")
        rstatus  `shouldBe` notFound404
        rversion `shouldBe` http11
        rbody `shouldBe` "(╯°□°）╯︵ ┻━┻)."
    describe "/user/add" $ do
      it "allows to add a user" $ \port -> do
        let user :: User = User "Alice" 1
        res <- try port $ userAdd user
        let userId :: UserId =
              case res of
                Right mUserId -> fromMaybe (UserKey 0) mUserId
                Left _ -> UserKey 0
        try port (userGet "Alice") `shouldReturn` Right (Entity userId user)
      it "allows to add two users" $ \port -> do
        let a = User "Alice" 1
        let b = User "Bob" 2
        _ <- try port $ userAdd a
        res <- try port $ userAdd b
        let userId :: UserId =
              case res of
                Right mUserId -> fromMaybe (UserKey 0) mUserId
                Left _ -> UserKey 0
        try port (userGet "Bob") `shouldReturn` Right (Entity userId b)
      it "returns Nothing when adding the same user twice" $ \port -> do
        let a = User "Alice" 1
        _ <- try port $ userAdd a
        try port (userAdd a) `shouldReturn` Right Nothing
    describe "/posts" $
      it "allows to add, update and delete a post" $ \port -> do
        let _ :<|> addC :<|> withIdClient = postClient
        let post = Post "title1" "body1" MP.dummyTime
        res <- try port $ addC post
        let postId :: MP.PostId =
              case res of
                Right mPostId -> fromMaybe (MP.PostKey 0) mPostId
                Left _ -> MP.PostKey 0
        -- get
        let getC :<|> updateC :<|> deleteC = withIdClient postId
        Right (Entity _ resPost) <- try port getC
        title resPost  `shouldBe` title post
        body resPost  `shouldBe` body post

        -- update
        let newPost = Post "title2" "body2" MP.dummyTime
        _ <- try port $ updateC newPost
        Right (Entity _ resNewPost) <- try port getC
        title resNewPost  `shouldBe` title newPost
        body resNewPost  `shouldBe` body newPost

        -- delete
        _ <- try port deleteC
        Left (FailureResponse (Response rstatus _ rversion rbody)) <- try port getC
        rstatus  `shouldBe` notFound404
        rversion `shouldBe` http11
        rbody `shouldBe` "(╯°□°）╯︵ ┻━┻)."

withApp :: (Int -> IO a) -> IO a
withApp action =
  inTempDirectory $ do
    app <- mkApp $ Config 3000 "sqlite.db"
    testWithApplication (return app) action

try :: Int -> ClientM a -> IO (Either ServantError a)
try port action = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  runClientM action $ ClientEnv manager baseUrl Nothing
