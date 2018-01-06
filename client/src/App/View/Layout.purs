module App.View.Layout where

import App.Events (Event(..))
import App.Routes (Route(..), toURL)
import App.State (State(..))
import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import App.View.Post.Post as Post
import App.View.Post.Posts as Posts
import CSS (CSS, backgroundColor, borderRadius, color, display, fontSize, fromString, inlineBlock, key, marginTop, padding, px, value, (?))
import CSS.Text (textDecoration, noneTextDecoration, letterSpacing)
import CSS.Text.Transform (textTransform, uppercase)
import CSS.TextAlign (center, textAlign)
import Color (rgb)
import Control.Bind (discard)
import Data.Function (($), (#))
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (a, div, li)
import Text.Smolder.HTML.Attributes (className, href)
import Text.Smolder.Markup (text, (!), (#!))

view :: State -> HTML Event
view (State st) =
  div ! className "app" $ do
    style css
    li $ a ! href "/users" #! onClick (Navigate "/users") $ text "User"
    li $ a ! href (toURL RPosts) #! onClick (Navigate $ toURL RPosts) $ text "Post"

    case st.route of
      (Home) -> Homepage.view (State st)
      (RPosts) -> Posts.view st.postChild
      (RPost postid) -> Post.view st.postChild
      (NotFound url) -> NotFound.view (State st)

css :: CSS
css = do
  let green = rgb 14 196 172
      blue = rgb 14 154 196
      white = rgb 250 250 250

  fromString "body" ? do
    backgroundColor (rgb 0 20 30)
    key (fromString "font-family") (value "-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen-Sans,Ubuntu,Cantarell,\"Helvetica Neue\",sans-serif")
    color white
    textAlign center

  fromString "h1" ? do
    fontSize (48.0 #px)
    marginTop (48.0 #px)
    textTransform uppercase
    letterSpacing (6.0 #px)

  fromString "a" ? do
    display inlineBlock
    borderRadius (2.0 #px) (2.0 #px) (2.0 #px) (2.0 #px)
    padding (6.0 #px) (6.0 #px) (6.0 #px) (6.0 #px)
    textDecoration noneTextDecoration
