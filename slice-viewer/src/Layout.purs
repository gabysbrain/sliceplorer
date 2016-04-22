module App.Layout where

import App.Counter as Counter
import App.FunctionSlices as Slices
--import AjaxExample.Todos as Todo
import App.Routes (Route(Home, NotFound))
import Network.HTTP.Affjax (AJAX)
import Prelude (($), map)
import Pux (EffModel, noEffects, mapState, mapEffects)
import Pux.Html (Html, div, h1, p, text)

data Action
  = Child (Counter.Action)
  | Child2 (Slices.Action)
  | PageView Route

type State =
  { route :: Route
  , count :: Counter.State 
  , slices :: Slices.State
  }

init :: State
init =
  { route: NotFound
  , count: Counter.init 
  , slices: Slices.init
  }

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (PageView route) state = 
  noEffects $ state { route = route }
update (Child action) state = 
  noEffects $ state { count = Counter.update action state.count }
update (Child2 action) state = 
   mapEffects Child2 (mapState (\s -> state {slices=s}) (Slices.update action state.slices))

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Pux Starter App" ]
    , map Child2 $ Slices.view state.slices
    --, map Child $ Todo.view Todo.init
    {--, case state.route of--}
        {--Home -> map Child $ Counter.view state.count--}
        {--NotFound -> App.NotFound.view state--}
    ]
