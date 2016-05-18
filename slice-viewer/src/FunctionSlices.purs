module App.FunctionSlices where

import Prelude 
import Data.Maybe (Maybe(Just, Nothing))
import Data.Maybe.Unsafe (fromJust)
import Data.Either (Either(..), either)
import Data.Either.Unsafe (fromRight)
import Data.Array (length, (..), head, slice)
import Data.Int (fromString, toNumber)
import Data.Tuple (fst, snd)
import Data.StrMap (lookup)
import Data.Foldable (sum, maximum, minimum)
import Math (min)
import Control.Monad.Eff.Exception (Error, error)
import Network.HTTP.Affjax (AJAX)

import Pux (EffModel, noEffects)
import Pux.Html (Html, a, div, span, button, input, text, p, select, option)
import Pux.Html.Events (onChange, onClick, FormEvent)
import Pux.Html.Attributes (className, selected, value, href)

import Data.Samples (SampleGroup(..), DimSamples(..),
                     jsonSamples, sortBy, metricNames)
import Data.Slices (Slice(..))
import App.Pager as Pager
import App.SampleView as SampleView
import App.Overview as Overview

type State =
  { d :: Int
  , function :: String
  , sliceMetrics :: Array String
  , error :: Maybe Error
  , samples :: Maybe SampleGroup
  , overview :: Maybe Overview.State
  }

data Action 
  = RequestSamples 
  | UpdateSamples (Either Error SampleGroup)
  | DimChange FormEvent
  | FunctionChange FormEvent
  | OverviewView Overview.Action

init :: State
init = 
  { d: 2
  , function: "spherical"
  , sliceMetrics: []
  , error: Nothing
  , samples: Nothing
  , overview: Nothing
  }

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (RequestSamples) state =
  { state: state {samples=Nothing}
  , effects: [ do
      samples <- jsonSamples state.function state.d
      return $ UpdateSamples samples
    ]
  }
update (UpdateSamples (Left err)) state =
  noEffects $ state {error = Just err}
update (UpdateSamples (Right s@(SampleGroup xs))) state =
  noEffects $ state { samples = Just s
                    , sliceMetrics = metricNames (fromJust $ head xs)
                    , overview = Just (Overview.init s)
                    , error = Nothing
                    }
update (OverviewView action) state =
  noEffects $ case state.overview of
                   Nothing -> state
                   Just o -> state {overview=Just (Overview.update action o)}

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
    [ div [className "data-controls"] 
        [ dimSelector state.d
        , funcSelector state.function
        , button [onClick (const RequestSamples)] [text "Fetch samples"]
        ]
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
viewSamples {overview=Just o} =
  div [className "samples"]
    [ map OverviewView $ Overview.view o ]
    
