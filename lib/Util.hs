{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}

module Util where

import Database.Persist.Sql
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.Reader (liftIO, asks, MonadIO, MonadReader)

import Environment

type SqlPersistM' = SqlPersistT (ResourceT IO)

runSql :: (MonadReader Environment m, MonadIO m) => SqlPersistM' b -> m b
runSql query = do
    pool <- asks pool
    liftIO $ runResourceT $ runSqlPool query pool
