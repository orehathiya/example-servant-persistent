module App.View.Post.Post where

import App.Events (Event(..)) as AEvent
import App.Events.Post (Event(..), State(..))
import Data.Function (const, ($))
import Data.Maybe (Maybe(..))
import Database.Persist.Class.PersistEntity (Entity(Entity))
import Model.Post (Post(..))
import Prelude (discard)
import Pux.DOM.Events (onChange, onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (button, div, h1, input)
import Text.Smolder.HTML.Attributes (type', value)
import Text.Smolder.Markup (text, (!), (#!))

view :: State -> HTML AEvent.Event
view (State st) =
  div do
    h1 $ text "Posts"
    case st.post of
      Just (Entity {entityKey: key, entityVal: Post post}) -> do
        if st.editing then do
          input ! type' "text" ! value st.title #! onChange \e -> (AEvent.ChildPostEvent $ TitleChange e)
          input ! type' "text" ! value st.body #! onChange \e -> (AEvent.ChildPostEvent $ BodyChange e)
          button #! onClick (const $ AEvent.ChildPostEvent $ UpdatePost (Entity {entityKey: key, entityVal: Post post {title = st.title, body = st.body}})) $ text "update"
          else do
            text post.title
            text post.body
            button #! onClick (const $ AEvent.ChildPostEvent$ EditPost (Post post)) $ text "edit"
      Nothing -> text "Nothing post"
