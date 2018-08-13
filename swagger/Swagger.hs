{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Api
import GHC.Generics
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Swagger
import Database.Persist.Types
import Model.User
import Model.Post
import Model.Report
import Servant.Swagger

main :: IO ()
main = writeSwaggerJSON

newtype MyKey a =
  Key Int
  deriving (Generic)

instance Generic (Key a) where
  type Rep (Key a) = Rep (MyKey a)
  from = from
  to = to

instance ToSchema (Entity User)
instance ToSchema User
instance ToSchema UserId
instance ToParamSchema UserId
instance ToSchema (Entity Post)
instance ToSchema Post
instance ToSchema PostId
instance ToParamSchema PostId
instance ToSchema (Entity Report)
instance ToSchema Report
instance ToSchema ReportId
instance ToParamSchema ReportId

apiSwagger :: Swagger
apiSwagger = toSwagger api

writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty apiSwagger)
