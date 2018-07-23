module App.View.Homepage where

import App.Events (Event(RequestUser))
import App.State (State(..))
import Control.Bind (discard)
import Data.Function (($), const)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Model.User (User(..))
import Database.Persist.Class.PersistEntity (Entity(..))
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (button, div, h1)
import Text.Smolder.Markup (text, (#!))

view :: State -> HTML Event
view (State st) =
  div do
    h1 $ text "Homepage"
    button #! onClick (const RequestUser) $ text "Fetch user"
    case st.user of
      Just (Entity {entityKey: _, entityVal: User user}) -> do
        text user.name
        text $ show user.age
      Nothing -> text "nothing user"
