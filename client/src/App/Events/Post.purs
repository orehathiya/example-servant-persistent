module App.Events.Post where

import ServerAPI

import App.Config (MySettings)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Database.Persist.Class.PersistEntity (Entity, Key)
import Model.Post (Post(..))
import Network.HTTP.Affjax (AJAX)
import Prelude (bind, map, pure, ($), (<$>))
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent, targetValue)
import Servant.PureScript.Affjax (AjaxError)
import Signal.Channel (CHANNEL)

data Event = ReceivePosts (Array (Entity Post))
           | ReceivePost (Entity Post)
           | PostSubmit
           | PostSubmited (Maybe (Key Post))
           | TitleChange DOMEvent
           | BodyChange DOMEvent
           | ReportError AjaxError

newtype State = State
  { posts :: Array (Entity Post)
  , post :: Maybe (Entity Post)
  , title :: String
  , body :: String
  , settings :: MySettings
    }

foldp :: forall fx. Event -> State -> EffModel State Event (ajax :: AJAX, dom:: DOM, history :: HISTORY | fx)
foldp (ReceivePosts posts) (State st) =
  noEffects $ State st {
    posts = posts
  }
foldp (ReceivePost post) (State st) =
  noEffects $ State st {
    post = Just post
  }
foldp (PostSubmited _) state = noEffects $ state
--onlyEffects state [pure $ Just $ AEvent.PageView RPosts]
foldp (PostSubmit) (State st) =
  runEffectActions
  (State st)
  [PostSubmited <$> postPosts (Post {title: st.title, body: st.body, created: Nothing})]

foldp (TitleChange ev) (State st) =
  noEffects $ State st {
    title = targetValue ev
  }
foldp (BodyChange ev) (State st) =
  noEffects $ State st {
    body = targetValue ev
  }
foldp (ReportError err) (State st) =
  noEffects $ State st

type APIEffect eff = ReaderT MySettings (ExceptT AjaxError (Aff ( ajax :: AJAX, channel :: CHANNEL, exception :: EXCEPTION | eff)))

runEffectActions :: forall fx. State -> Array (APIEffect (fx) Event) -> EffModel State Event (ajax :: AJAX | fx)
runEffectActions (State st) effects = { state : State st, effects : map (runEffect st.settings) effects }

runEffect :: forall fx. MySettings -> APIEffect (fx) Event -> Aff (channel :: CHANNEL, ajax :: AJAX, exception :: EXCEPTION | fx) (Maybe Event)
runEffect settings m = do
    er <- runExceptT  $ runReaderT m settings
    case er of
      Left err -> pure $ Just $ ReportError err
      Right v -> pure $ Just $ v
