module App.State where

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
  { route :: Route
  , user :: Maybe (Entity User)
  , posts :: Array (Entity Post)
  , post :: Maybe (Entity Post)
  , title :: String
  , body :: String
  , status :: String
  , loaded :: Boolean
  , settings :: MySettings
  , lastError :: Maybe AjaxError
  }

init :: MySettings -> String -> State
init settings url =
  State {
     route: match url
   , user: Nothing
   , posts: []
   , post: Nothing
   , title: ""
   , body: ""
   , status: "Nothing loaded from server yet"
   , loaded: false
   , settings: settings
   , lastError: Nothing
  }
