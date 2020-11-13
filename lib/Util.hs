{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Util where

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Database.Persist.Sql
import Environment

type SqlPersistM' = SqlPersistT (ResourceT IO)

runSql :: (MonadReader Environment m, MonadIO m) => SqlPersistM' b -> m b
runSql query = do
  pool <- asks pool
  liftIO $ runResourceT $ runSqlPool query pool
