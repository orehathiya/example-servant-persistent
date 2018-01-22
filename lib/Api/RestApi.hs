{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.RestApi where

import Servant
import Servant.Client
import Database.Persist.Sql
import Data.Aeson

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

-- Build the appropriate 'Server'
-- given the handlers of the right type.
serverFor :: ConnectionPool
          -> (ConnectionPool -> Handler [Entity a]) -- handler for listing of 'a's
          -> (ConnectionPool -> a -> Handler (Maybe i)) -- handler for adding an 'a'
          -> (ConnectionPool -> i -> Handler (Entity a)) -- handler for viewing an 'a' given its identifier of type 'i'
          -> (ConnectionPool -> i -> a -> Handler NoContent) -- updating an 'a' with given id
          -> (ConnectionPool -> i -> Handler NoContent) -- deleting an 'a' given its id
          -> Server (APIFor a i)
serverFor pool getsH addH getH updateH deleteH =
       getsH pool
  :<|> addH pool
  :<|> (\i -> getH pool i :<|> updateH pool i :<|> deleteH pool i)

data ApiClient a i = ApiClient
  { getsC :: ClientM [Entity a]
  , addC :: a -> ClientM (Maybe i)
  , mkWithIdClient :: i -> WithIdClient a i
  }

data WithIdClient a i = WithIdClient
  { getC :: ClientM (Entity a)
  , updateC :: a -> ClientM NoContent
  , deleteC :: ClientM NoContent
  }

mkApiClient :: (ToJSON a, FromJSON i, FromJSON (Entity a), ToHttpApiData i) => ApiClient a i
mkApiClient = ApiClient{..}
  where
    apiClient = client (Proxy :: Proxy (APIFor a i))
    getsC :<|> addC :<|> withIdClient = apiClient

    mkWithIdClient id = WithIdClient{..}
      where
        getC :<|> updateC :<|> deleteC = withIdClient id
