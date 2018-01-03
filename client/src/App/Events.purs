module App.Events where

import ServerAPI

import App.Routes (Route(..), match)
import App.State (State(..), MySettings)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Database.Persist.Class.PersistEntity (Entity, Key(..))
import Model.Post (Post(..))
import Model.User (User)
import Network.HTTP.Affjax (AJAX)
import Prelude (bind, discard, map, pure, ($), (<$>), (=<<))
import Prim (Array, String)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent, targetValue)
import Servant.PureScript.Affjax (AjaxError)
import Signal.Channel (CHANNEL)

data Event = PageView Route
           | Navigate String DOMEvent
           | ReceivePosts (Array (Entity Post))
           | ReceivePost (Entity Post)
           | ReceiveUser (Entity User)
           | RequestUser
           | PostSubmit
           | PostSubmited (Maybe (Key Post))
           | TitleChange DOMEvent
           | BodyChange DOMEvent
           | ReportError AjaxError

foldp :: forall fx. Event -> State -> EffModel State Event (ajax :: AJAX, dom:: DOM, history :: HISTORY | fx)
foldp (PageView route) (State st) =
  case route of
    RPosts -> runEffectActions
             (State st {
               route = route
             , loaded = true
             })
             [ReceivePosts <$> getPosts]
    RPost postid -> runEffectActions
             (State st {
               route = route
             , loaded = true
             })
             [ReceivePost <$> getPostsByPostid (Key postid)]
    otherwise -> noEffects $ State st {
      route = route
    , loaded = true
    }
foldp (Navigate url ev) (State st) =
  { state: State st
  , effects: [
    liftEff do
      preventDefault ev
      h <- history =<< window
      pushState (toForeign {}) (DocumentTitle "") (URL url) h
      pure $ Just $ PageView (match url)
    ]

  }
foldp (ReceivePosts posts) (State st) =
  noEffects $ State st {
    posts = posts
  , status = "Posts"
  }
foldp (ReceivePost post) (State st) =
  noEffects $ State st {
    post = Just post
  , status = "Post"
  }
foldp (ReceiveUser user) (State st) =
  noEffects $ State st {
    user = Just user
  , status = "User"
  }
foldp (RequestUser) state = runEffectActions state [ReceiveUser <$> getUserGetByName "Alice"]
foldp (ReportError err) (State st) =
  noEffects $ State st {
    lastError = Just err
  }
foldp (PostSubmited _) state = onlyEffects state [pure $ Just $ PageView RPosts]
foldp (PostSubmit) (State st) = runEffectActions (State st) [PostSubmited <$> postPosts (Post {title: st.title, body: st.body})]
foldp (TitleChange ev) (State st) =
  noEffects $ State st {
    title = targetValue ev
  }
foldp (BodyChange ev) (State st) =
  noEffects $ State st {
    body = targetValue ev
  }

type APIEffect eff = ReaderT MySettings (ExceptT AjaxError (Aff ( ajax :: AJAX, channel :: CHANNEL, exception :: EXCEPTION | eff)))

runEffectActions :: forall fx. State -> Array (APIEffect (fx) Event) -> EffModel State Event (ajax :: AJAX | fx)
runEffectActions (State st) effects = { state : State st, effects : map (runEffect st.settings) effects }

runEffect :: forall fx. MySettings -> APIEffect (fx) Event -> Aff (channel :: CHANNEL, ajax :: AJAX, exception :: EXCEPTION | fx) (Maybe Event)
runEffect settings m = do
    er <- runExceptT  $ runReaderT m settings
    case er of
      Left err -> pure $ Just $ ReportError err
      Right v -> pure $ Just $ v
