{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.RestApi where

import Servant
import Database.Persist.Sql

import Server

-- API for values of type 'a'
-- indexed by values of type 'i'
type APIFor a i =
       Get '[JSON] [Entity a] -- list 'a's
  :<|> ReqBody '[JSON] a :> Post '[JSON] (Maybe i) -- add an 'a'
  :<|> Capture "id" i :>
         ( Get '[JSON] (Entity a) -- view an 'a' given its "identifier" of type 'i'
      :<|> ReqBody '[JSON] a :> PutNoContent '[JSON] NoContent -- update an 'a'
      :<|> DeleteNoContent '[JSON] NoContent -- delete an 'a'
         )

serverFor :: AppHandler [Entity a] -- handler for listing of 'a's
          -> (a -> AppHandler (Maybe i)) -- handler for adding an 'a'
          -> (i -> AppHandler (Entity a)) -- handler for viewing an 'a' given its identifier of type 'i'
          -> (i -> a -> AppHandler NoContent) -- updating an 'a' with given id
          -> (i -> AppHandler NoContent) -- deleting an 'a' given its id
          -> AppServer (APIFor a i)
serverFor getsH addH getH updateH deleteH =
       getsH
  :<|> addH
  :<|> (\i -> getH i :<|> updateH i :<|> deleteH i)
