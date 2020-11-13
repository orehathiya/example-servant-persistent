{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Api
import Api.Report
import Api.User
import Control.Lens.Operators
import Data.Proxy
import Servant.Docs

apiDocs :: API
apiDocs = docsWith defaultDocOptions [] extra api

extra :: ExtraInfo Api
extra =
  mconcat
    [ extraInfo (Proxy :: Proxy UserGet) $
        defAction & notes <>~ [DocNote "Title" ["This is a pen"]],
      extraInfo (Proxy :: Proxy ReportGet) defAction
    ]

main :: IO ()
main = writeFile "doc.md" $ markdown apiDocs
