module Main where

import Api
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Swagger
import Servant.Swagger

main :: IO ()
main = writeSwaggerJSON

apiSwagger :: Swagger
apiSwagger = toSwagger api

writeSwaggerJSON :: IO ()
writeSwaggerJSON = BL8.writeFile "swagger.json" (encodePretty apiSwagger)
