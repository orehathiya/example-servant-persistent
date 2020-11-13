{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Report where

import Database.Persist
import Model.Report
import Servant.API

type ReportsApi = ReportGet

type ReportGet = "report" :> Get '[JSON] [Entity Report]
