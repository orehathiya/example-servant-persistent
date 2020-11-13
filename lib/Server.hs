module Server where

import Control.Monad.Trans.Reader (ReaderT)
import Environment
import Servant

type AppHandler = ReaderT Environment Handler

type AppServer api = ServerT api AppHandler
