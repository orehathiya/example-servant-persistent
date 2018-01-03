module App.View.Posts where

import App.Events (Event(..))
import App.Routes (toURL, Route(..))
import App.State (State(..))
import Data.Foldable (for_)
import Data.Function (($))
import Database.Persist.Class.PersistEntity (Entity(..), Key(..))
import Model.Post (Post(..))
import Prelude (discard)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, h1, li, ul, a)
import Text.Smolder.HTML.Attributes (href)
import Text.Smolder.Markup (text, (!), (#!))

view :: State -> HTML Event
view (State st) =
  div do
    h1 $ text "Posts"
    ul $ for_ st.posts post

post :: Entity Post -> HTML Event
post (Entity {key: (Key postid), value: Post post}) =
  li $ a ! href (toURL (RPost postid)) #! onClick (Navigate (toURL (RPost postid))) $ text post.title
