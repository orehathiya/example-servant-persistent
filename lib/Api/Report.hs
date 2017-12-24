{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Report where

import Servant.API
import Database.Persist

import Model.Report

type ReportsApi = ReportGet

type ReportGet = "report" :> Get '[JSON] [Entity Report]
