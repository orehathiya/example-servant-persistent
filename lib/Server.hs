module Server where

import Servant

import Control.Monad.Trans.Reader (ReaderT)
import Environment

type AppHandler = ReaderT Environment Handler

type AppServer api = ServerT api AppHandler
