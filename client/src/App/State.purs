module App.State where

import App.Config (MySettings)
import App.Events.Post (State(..)) as EPost
import App.Routes (Route, match)
import Data.Maybe (Maybe(..))
import Database.Persist.Class.PersistEntity (Entity)
import Model.User (User)
import Servant.PureScript.Affjax (AjaxError)

newtype State = State
  { route :: Route
  , user :: Maybe (Entity User)
  , postChild :: EPost.State
  , settings :: MySettings
  , lastError :: Maybe AjaxError
  }

init :: MySettings -> String -> State
init settings url =
  State {
     route: match url
   , user: Nothing
   , postChild: EPost.State {
       posts: []
     , post: Nothing
     , title: ""
     , body: ""
     , settings: settings
     , lastError: Nothing
     }
   , settings: settings
   , lastError: Nothing
  }
