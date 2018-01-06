module App.Routes where

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.Generic (class Generic, gEq, gShow)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Pux.Router (end, router, lit, int)

data Route = Home | RPosts | RPost Int | NotFound String

derive instance genericRoute :: Generic Route

instance eqRoute :: Eq Route where
  eq = gEq

instance showRoute :: Show Route where
  show = gShow

match :: String -> Route
match url = fromMaybe (NotFound url) $ router url $
  Home <$ end
  <|>
  RPosts <$ (lit "posts") <* end
  <|>
  RPost <$> (lit "posts" *> int) <* end

toURL :: Route -> String
toURL (NotFound url) = url
toURL (Home) = "/"
toURL (RPosts) = "/posts"
toURL (RPost postid) = "/posts/" <> show postid
