module App.FunctionSlices where

import Prelude 
import Data.Maybe (Maybe(Just, Nothing))
import Data.Maybe.Unsafe (fromJust)
import Data.Either (Either(..), either)
import Data.Either.Unsafe (fromRight)
import Data.Array (length, (..), head)
import Data.Int (fromString)

import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (Error)
import Network.HTTP.Affjax (AJAX, get)

import Pux (EffModel, noEffects)
import Pux.Html (Html, div, span, button, input, text, p, select, option)
import Pux.Html.Events (onChange, onClick, FormEvent)
import Pux.Html.Attributes (className, selected, value)

import Data.Samples (SampleGroup(..), Samples(..), parse)
import Vis.Vega (vegaChart)

type State =
  { d :: Int
  , function :: String
  , error :: Maybe Error
  , samples :: Maybe SampleGroup
  }

data Action 
  = RequestSamples 
  | ReceiveSamples (Either Error SampleGroup)
  | DimChange FormEvent
  | FunctionChange FormEvent

init :: State
init = 
  { d: 2
  , function: "spherical"
  , error: Nothing
  , samples: Nothing
  }

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (RequestSamples) state =
  { state: state {samples=Nothing}
  , effects: [ do
      let url = "/data/" ++ state.function ++ "_" ++ (show state.d) ++ "_slices.csv"
      --res <- attempt $ get "/data/test.csv"
      res <- attempt $ get url
      --x = fromRight res
      let samples = either Left (\r -> Right $ parse r.response) res
      return $ ReceiveSamples samples
    ]
  }
update (ReceiveSamples (Left err)) state =
  noEffects $ state {error = Just err}
update (ReceiveSamples (Right s)) state =
  noEffects $ state {samples = Just s, error = Nothing}
update (DimChange ev) state = 
  -- TODO: maybe some error checking?
  { state: state {d=fromJust $ fromString ev.target.value}
  , effects: [ do
      return RequestSamples
    ]
  }
update (FunctionChange ev) state = 
  -- TODO: maybe some error checking?
  { state: state {function=ev.target.value}
  , effects: [ do
      return RequestSamples
    ]
  }


view :: State -> Html Action
view state = 
  div []
    [ viewControls state
    , viewError state.error
    , p [] [text $ show state.d]
    , p [] [text state.function]
    , viewSamples state.samples
    ]

viewControls :: State -> Html Action
viewControls state = 
  div [] 
    [ dimSelector state.d
    , funcSelector state.function
    , button [onClick (const RequestSamples)] [text "Fetch samples"]
    ]

dimSelector :: Int -> Html Action
dimSelector d =
  select [onChange DimChange] $
    map (\i -> option [value (show i), selected (i==d)] [text $ show i]) (2..10)

funcSelector :: String -> Html Action
funcSelector fname =
  select [onChange FunctionChange] $
    map (\f -> option [value f, selected (f==fname)] [text f])
      ["ackley", "rosenbrock", "spherical", "schwefel", "zakharov"]

viewError :: Maybe Error -> Html Action
viewError Nothing  = text ""
viewError (Just err) = span [className "error"] [text $ show err]

viewSamples :: Maybe SampleGroup -> Html Action
viewSamples Nothing = p [] [text "Nothing loaded"]
viewSamples (Just (SampleGroup s)) = 
  singleSlice $ (fromJust <<< head <<< fromJust <<< head) s

singleSlice :: Samples -> Html Action
singleSlice s = vegaChart [] [{"x": 1.0, "y": 1.0},
                              {"x": 2.0, "y": 2.0},
                              {"x": 3.0, "y": 3.0},
                              {"x": 4.0, "y": 4.0},
                              {"x": 5.0, "y": 5.0}]


