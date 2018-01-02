module App.Routes where

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Show (show)
import Pux.Router (end, router, lit, int)

data Route = Home | Posts | Post Int | NotFound String

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Home <$ end
  <|>
  Posts <$ (lit "#" *> lit "posts") <* end
  <|>
  Post <$> (lit "#" *> lit "posts" *> int) <* end

toURL :: Route -> String
toURL (NotFound url) = url
toURL (Home) = "/"
toURL (Posts) = "/#/posts"
toURL (Post postid) = "/#/posts/" <> show postid
