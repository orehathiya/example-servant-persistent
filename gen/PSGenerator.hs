{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Proxy
import Database.Persist.Types
import GHC.Generics
import Language.PureScript.Bridge
import Language.PureScript.Bridge.TypeParameters
import Language.PureScript.Bridge.PSTypes (psString)
import Servant.PureScript

import Api
import Model.Report
import Model.User
import Model.Post

utcTimeBridge :: BridgePart
utcTimeBridge = typeName ^== "UTCTime" >> return psString

myBridge :: BridgePart
myBridge = defaultBridge <|> utcTimeBridge

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge

data MyEntity record = Entity
  { key :: Key record
  , value :: record
  } deriving (Generic)

newtype MyKey a =
  Key Int
  deriving (Generic)

instance Generic (Entity a) where
  type Rep (Entity a) = Rep (MyEntity a)
  from = from
  to = to

instance Generic (Key a) where
  type Rep (Key a) = Rep (MyKey a)
  from = from
  to = to

myTypes :: [SumType 'Haskell]
myTypes =
  [ mkSumType (Proxy :: Proxy (Entity A))
  , mkSumType (Proxy :: Proxy (Key A))
  , mkSumType (Proxy :: Proxy User)
  , mkSumType (Proxy :: Proxy Report)
  , mkSumType (Proxy :: Proxy Post)
  ]

main :: IO ()
main = do
  let frontEndRoot = "client/src"
  writeAPIModule frontEndRoot myBridgeProxy api
  writePSTypes frontEndRoot (buildBridge myBridge) myTypes
