module App.View.Post.Posts where

import App.Events (Event(..)) as AEvent
import App.Events.Post (Event(..), State(..))
import App.Routes (toURL, Route(..))
import Data.Foldable (for_)
import Data.Function (const, ($))
import Database.Persist.Class.PersistEntity (Entity(..), Key(..))
import Model.Post (Post(..))
import Prelude (discard)
import Pux.DOM.Events (onClick, onChange, onSubmit)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, h1, li, ul, a, form, input, button)
import Text.Smolder.HTML.Attributes (href, name, type', value, action)
import Text.Smolder.Markup (text, (!), (#!))

view :: State -> HTML AEvent.Event
view (State st) =
  div do
    h1 $ text "Posts"
    ul $ for_ st.posts postView
    postForm (State st)

postView :: Entity Post -> HTML AEvent.Event
postView (Entity {key: (Key postid), value: Post post}) = do
  li $ a ! href (toURL (RPost postid)) #! onClick (AEvent.Navigate (toURL (RPost postid))) $ text post.title
  button #! onClick (const $ AEvent.ChildPostEvent $ DeletePost (Key postid)) $ text "delete"

postForm :: State -> HTML AEvent.Event
postForm (State st) =
  form ! name "post" ! action "javascript:void(0);" #! onSubmit (const $ AEvent.ChildPostEvent PostSubmit) $ do
    input ! type' "text" ! value st.title #! onChange \e -> (AEvent.ChildPostEvent $ TitleChange e)
    input ! type' "text" ! value st.body #! onChange \e -> (AEvent.ChildPostEvent $ BodyChange e)
    button ! type' "submit" $ text "submit"
