module App.View.Post where

import App.Events (Event)
import App.State (State(..))
import Data.Function (($))
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, h1)
import Text.Smolder.Markup (text)

view :: State -> HTML Event
view (State st) =
  div do
    h1 $ text "Posts"
