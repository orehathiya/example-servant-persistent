{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens.Operators
import Data.Proxy
import Servant.Docs

import Api
import Api.Report
import Api.User

apiDocs :: API
apiDocs = docsWith defaultDocOptions [] extra api

extra :: ExtraInfo Api
extra =
  mconcat
    [ extraInfo (Proxy :: Proxy UserGet) $
      defAction & notes <>~ [DocNote "Title" ["This is a pen"]]
    , extraInfo (Proxy :: Proxy ReportGet) defAction
    ]

main :: IO ()
main = writeFile "doc.md" $ markdown apiDocs
