{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Server.Report where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Text hiding (map)
import Database.Persist.Sql

import Api.Report
import Model.Report
import Environment
import Server

reportsServer :: AppServer ReportsApi
reportsServer = reportGetH

reportGetH :: AppHandler [Entity Report]
reportGetH = do
  pool <- asks pool
  liftIO $ reportGet pool
  where
    reportGet :: ConnectionPool -> IO [Entity Report]
    reportGet pool =
      flip runSqlPersistMPool pool $ do
        mReport :: [(Single (BackendKey SqlBackend), Single Int, Single Int, Single Int)] <-
          rawSql
            (pack
               "select id, report.imp as imp, report.click as click, report.click / report.imp as ctr from report")
            []
        return $ map convertTuple mReport

convertTuple :: (Single (BackendKey SqlBackend), Single Int, Single Int, Single Int) -> Entity Report
convertTuple (id, imp, click, ctr) =
  Entity (ReportKey $ unSingle id) (Report (unSingle imp) (unSingle click) (unSingle ctr))
