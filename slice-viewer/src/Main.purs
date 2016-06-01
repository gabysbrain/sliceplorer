module Main where

import Prelude (bind, return)
import App.Routes (match)
import Control.Bind ((=<<))
import App.Layout (Action(PageView), State, view, update)
import Control.Monad.Eff (Eff)
import Pux (App, Config, CoreEffects, renderToDOM)
import Pux.Router (sampleUrl)
import Signal ((~>))
import App.Core (AppEffects)
import DOM (DOM)

-- | App configuration
config :: forall eff.
          State ->
          Eff (dom :: DOM | eff) (Config State Action AppEffects)
config state = do
  -- | Create a signal of URL changes.
  urlSignal <- sampleUrl
  		  
  -- | Map a signal of URL changes to PageView actions.
  let routeSignal = urlSignal ~> \r -> PageView (match r)

  return 
    { initialState: state
    , update: update
    , view: view
    , inputs: [routeSignal]
    --, inputs: []
    }

-- | Entry point for the browser.
main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  app <- Pux.start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  return app

-- | Entry point for the browser with pux-devtool injected.
debug :: State -> Eff (CoreEffects AppEffects) (App State (Pux.Devtool.Action Action))
debug state = do
  app <- Pux.Devtool.start =<< config state
  renderToDOM "#app" app.html
  -- | Used by hot-reloading code in support/index.js
  return app

