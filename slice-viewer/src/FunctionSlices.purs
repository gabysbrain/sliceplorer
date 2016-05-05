module App.FunctionSlices where

import Prelude 
import Data.Maybe (Maybe(Just, Nothing))
import Data.Maybe.Unsafe (fromJust)
import Data.Either (Either(..), either)
import Data.Either.Unsafe (fromRight)
import Data.Array (length, (..), head, slice)
import Data.Int (fromString)
import Data.Tuple (fst, snd)
import Math (min)
import Control.Monad.Eff.Exception (Error)

import Control.Monad.Aff (attempt)
import Network.HTTP.Affjax (AJAX, get)

import Pux (EffModel, noEffects)
import Pux.Html (Html, a, div, span, button, input, text, p, select, option)
import Pux.Html.Events (onChange, onClick, FormEvent)
import Pux.Html.Attributes (className, selected, value, href)

import Data.Samples (SampleGroup(..), DimSamples(..), parseJson)
import App.Pager as Pager
import App.SampleView as SampleView

type State =
  { d :: Int
  , function :: String
  , error :: Maybe Error
  , samples :: Maybe SampleGroup
  , pager :: Pager.State
  }

data Action 
  = RequestSamples 
  | ReceiveSamples (Either Error SampleGroup)
  | DimChange FormEvent
  | FunctionChange FormEvent
  | PagerView Pager.Action

init :: State
init = 
  { d: 2
  , function: "spherical"
  , error: Nothing
  , samples: Nothing
  , pager: Pager.init SampleView.view
  }

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (RequestSamples) state =
  { state: state {samples=Nothing}
  , effects: [ do
      let url = "http://localhost:5000/slice/" ++ state.function ++ "/" ++ (show state.d)
      --res <- attempt $ get "/data/test.csv"
      res <- attempt $ get url
      let samples = case res of
                        Right r  -> parseJson r.response
                        Left err -> Left err
      return $ ReceiveSamples samples
    ]
  }
update (ReceiveSamples (Left err)) state =
  noEffects $ state {error = Just err}
update (ReceiveSamples (Right s@(SampleGroup xs))) state =
  { state: state {samples = Just s
                 , error = Nothing
                 }
  , effects: [do
      return $ PagerView (Pager.ChangeChildren xs)
    ]
  }
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
update (PagerView action) state =
  noEffects $ state {pager=Pager.update action state.pager}

view :: State -> Html Action
view state = 
  div []
    [ viewControls state
    , viewError state.error
    , viewSamples state
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
  select [onChange DimChange, value (show d)] $
    map (\i -> option [value (show i)] [text $ show i]) (2..10)

funcSelector :: String -> Html Action
funcSelector fname =
  select [onChange FunctionChange, value fname] $
    map (\f -> option [value f] [text f])
      ["ackley", "rosenbrock", "spherical", "schwefel", "zakharov"]

viewError :: Maybe Error -> Html Action
viewError Nothing  = text ""
viewError (Just err) = div [className "error"] [text $ show err]

viewSamples :: State -> Html Action
viewSamples {samples=Nothing} = p [] [text "Nothing loaded"]
{--viewSamples (Just (SampleGroup s)) mnVal mxVal = --}
  {--div [className "sample-group"] $ map viewSample (slice mnVal mxVal s)--}
  --[ viewSample $ (fromJust <<< head) s ]
viewSamples state@{samples=Just (SampleGroup s)} = 
  div [className "samples"] 
    [ map PagerView $ Pager.view state.pager
    ]

