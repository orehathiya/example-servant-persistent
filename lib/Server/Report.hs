{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Server.Report where

import Data.Text hiding (map)
import Database.Persist.Sql

import Api.Report
import Model.Report
import Server
import Util

reportsServer :: AppServer ReportsApi
reportsServer = reportGetH

reportGetH :: AppHandler [Entity Report]
reportGetH =
  runSql $ do
    mReport :: [(Single (BackendKey SqlBackend), Single Int, Single Int, Single Int)] <-
      rawSql
        (pack
           "select id, report.imp as imp, report.click as click, report.click / report.imp as ctr from report")
        []
    return $ map convertTuple mReport

convertTuple :: (Single (BackendKey SqlBackend), Single Int, Single Int, Single Int) -> Entity Report
convertTuple (id, imp, click, ctr) =
  Entity
    (ReportKey $ unSingle id)
    (Report (unSingle imp) (unSingle click) (unSingle ctr))
