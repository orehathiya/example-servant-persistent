module Main where

import Prelude

import App.Events (Event(PageView), foldp)
import App.Routes (match)
import App.State (State, MySettings, init)
import App.View.Layout (view)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY)
import Network.HTTP.Affjax (AJAX)
import Pux (CoreEffects, start)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM)
import Servant.PureScript.Settings (defaultSettings)
import ServerAPI (SPParams_(..))
import Signal ((~>))

main :: Eff (CoreEffects (ajax :: AJAX, history :: HISTORY, dom :: DOM)) Unit
main = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleURL =<< window

  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)
  let settings = defaultSettings $ SPParams_ {baseURL : "http://localhost:3000/"}
  -- | Start the app.
  app <- start
    { initialState: initialState settings
    , view
    , foldp
    , inputs: [routeSignal] }

  -- | Render to the DOM
  renderToDOM "#app" app.markup app.input

initialState :: MySettings -> State
initialState settings = init settings "/"
