module App.Layout where

import App.FunctionSlices as Slices
import App.Routes (Route(Home, NotFound))
import Network.HTTP.Affjax (AJAX)
import Prelude (($), map)
import Pux (EffModel, noEffects, mapState, mapEffects)
import Pux.Html (Html, div, h1, p, text)

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

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (PageView route) state = 
  noEffects $ state { route = route }
update (SliceView action) state = 
   mapEffects SliceView (mapState (\s -> state {slices=s}) (Slices.update action state.slices))

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Slice viewer" ]
    , map SliceView $ Slices.view state.slices
    {--, case state.route of--}
        {--Home -> map Child $ Counter.view state.count--}
        {--NotFound -> App.NotFound.view state--}
    ]

