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
import Text.Smolder.HTML (a, div, h1, button)
import Text.Smolder.HTML.Attributes (href, className)
import Text.Smolder.Markup ((!), (#!), text)

view :: State -> HTML Event
view (State st) =
  div do
    h1 $ text "Pux"
    a ! className "guide" ! href "https://www.purescript-pux.org/" $ text "Guide"
    a ! className "github" ! href "https://github.com/alexmingoia/purescript-pux/" $ text "GitHub"
    button #! onClick (const RequestUser) $ text "Fetch user"
    case st.user of
      Just (Entity entity) -> do
        let User user = entity.value
        text user.name
        text $ show user.age
      Nothing -> text "nothing user"
