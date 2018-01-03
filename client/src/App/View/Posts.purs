module App.View.Posts where

import App.Events (Event(..))
import App.Routes (toURL, Route(..))
import App.State (State(..))
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

view :: State -> HTML Event
view (State st) =
  div do
    h1 $ text "Posts"
    ul $ for_ st.posts post
    postForm (State st)

post :: Entity Post -> HTML Event
post (Entity {key: (Key postid), value: Post post}) =
  li $ a ! href (toURL (RPost postid)) #! onClick (Navigate (toURL (RPost postid))) $ text post.title

postForm :: State -> HTML Event
postForm (State st) =
  form ! name "post" ! action "javascript:void(0);" #! onSubmit (const PostSubmit) $ do
    input ! type' "text" ! value st.title #! onChange TitleChange
    input ! type' "text" ! value st.body #! onChange BodyChange
    button ! type' "submit" $ text "submit"
