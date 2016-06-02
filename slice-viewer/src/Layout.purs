module App.Layout where

import App.FunctionSlices as Slices
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux (EffModel, mapState, mapEffects, noEffects)
import Pux.Html (Html, div, h1, text)
import App.Core (AppEffects)
import Debug.Trace

data Action
  = SliceView (Slices.Action)
  | PageView Route

type State =
  { route :: Route
  , slices :: Slices.State
  }

init :: State
init =
  { route: NotFound
  , slices: Slices.init
  }

update :: Action -> State -> EffModel State Action AppEffects
update (PageView Home) state =
  update (SliceView Slices.RequestDatasets) state'
  where
  state' = state {route=Home}
update (PageView route) state = 
  noEffects $ state { route = route }
update (SliceView action) state = 
   mapEffects SliceView (mapState (\s -> state {slices=s}) (Slices.update action state.slices))

view :: State -> Html Action
view state@{route=Home} =
  div
    []
    [ h1 [] [ text "Slice viewer" ]
    , map SliceView $ Slices.view state.slices
    ]
view state@{route=NotFound} = App.NotFound.view state

