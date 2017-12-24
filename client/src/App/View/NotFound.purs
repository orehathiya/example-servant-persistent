module App.View.NotFound where

import App.Events (Event(..))
import App.State (State)
import Control.Bind (discard)
import Data.Function (($))
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, h2)
import Text.Smolder.HTML.Attributes (href)
import Text.Smolder.Markup ((!), (#!), text)

view :: State -> HTML Event
view st = do
  div $ h2 $ text "404 Not Found"
  a ! href "/" #! onClick (Navigate "/") $ text "Home"
