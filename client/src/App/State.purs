module App.State where

import App.Config (config)
import App.Routes (Route, match)
import Data.Maybe (Maybe(..))
import Database.Persist.Class.PersistEntity (Entity)
import Model.User (User)
import Model.Post (Post)
import Servant.PureScript.Settings (SPSettings_)
import ServerAPI (SPParams_)
import Servant.PureScript.Affjax (AjaxError)

type MySettings = SPSettings_ SPParams_

newtype State = State
  { title :: String
  , route :: Route
  , user :: Maybe (Entity User)
  , posts :: Array (Entity Post)
  , post :: Maybe (Entity Post)
  , status :: String
  , loaded :: Boolean
  , settings :: MySettings
  , lastError :: Maybe AjaxError
  }

init :: MySettings -> String -> State
init settings url =
  State {
     title: config.title
   , route: match url
   , user: Nothing
   , posts: []
   , post: Nothing
   , status: "Nothing loaded from server yet"
   , loaded: false
   , settings: settings
   , lastError: Nothing
  }
