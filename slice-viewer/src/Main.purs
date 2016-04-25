module Main where

import Prelude (bind, return, ($), (==))
import Data.Maybe (Maybe(..), fromMaybe)
import App.Routes (match)
import App.Layout (Action(PageView), State, view, update)
import Control.Monad.Eff (Eff)
import Debug.Trace (traceAny)
import DOM (DOM)
import Pux (App, CoreEffects, fromSimple, renderToDOM)
import Pux.Router (sampleUrl)
import Signal ((~>))
--import Node.Process (lookupEnv)

type AppEffects = (dom :: DOM)

-- | Entry point for the browser.
--main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  -- | Create a signal of URL changes.
  --urlSignal <- sampleUrl

  -- | Map a signal of URL changes to PageView actions.
  --let routeSignal = urlSignal ~> \r -> PageView (match r)

  -- TODO: turn of devtools when compiling in production mode
  app <- Pux.Devtool.start
  --app <- Pux.start
    { initialState: state
    , update:
        -- | Logs all actions and states (removed in production builds).
        (\a s -> traceAny {action: a, state: s} (\_ -> update a s))
        --(\a s -> traceAny {action: a, state: s} (\_ -> (fromSimple update) a s))
    , view: view
    --, inputs: [routeSignal] }
    , inputs: [] }

  renderToDOM "#app" app.html

  -- | Used by hot-reloading code in support/index.js
  return app
