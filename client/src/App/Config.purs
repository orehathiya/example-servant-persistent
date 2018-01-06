module App.Config where

import Servant.PureScript.Settings (SPSettings_)
import ServerAPI (SPParams_)

type Config =
  { title :: String
  , public_path :: String
  }

foreign import config :: Config

type MySettings = SPSettings_ SPParams_
