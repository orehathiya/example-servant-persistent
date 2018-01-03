module App.View.Post where

import App.Events (Event)
import App.State (State(..))
import Data.Maybe (Maybe(..))
import Data.Function (($))
import Database.Persist.Class.PersistEntity (Entity(Entity))
import Model.Post (Post(..))
import Prelude (discard)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, h1)
import Text.Smolder.Markup (text)

view :: State -> HTML Event
view (State st) =
  div do
    h1 $ text "Posts"
    case st.post of
      Just (Entity {key: _, value: Post post}) -> do
        text post.title
        text post.body
      Nothing -> text "Nothing post"
