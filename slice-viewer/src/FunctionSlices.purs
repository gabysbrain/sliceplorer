module App.FunctionSlices where

import Prelude hiding (div)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Maybe.Unsafe (fromJust)
import Data.Either (Either(..))
import Data.Array ((..), head)
import Data.Int (fromString)
import Control.Monad.Eff.Exception (Error)
import Network.HTTP.Affjax (AJAX)

import Pux (EffModel, noEffects)
import Pux.Html (Html, div, button, text, p, select, option, input)
import Pux.Html.Events (onChange, onClick, FormEvent)
import Pux.Html.Attributes (className, disabled, value, type_, min, max, step)

import Data.Samples (SampleGroup(..), jsonSamples, metricNames)
import App.Overview as Overview

type State =
  { d :: Int
  , function :: String
  , sampleLimit :: Int
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
  | UpdateSampleLimit FormEvent
  | OverviewView Overview.Action

init :: State
init = 
  { d: 2
  , function: "spherical"
  , sampleLimit: 50
  , sliceMetrics: []
  , error: Nothing
  , samples: Nothing
  , overview: Nothing
  }

-- FIXME: ideally unifying this type is handled at a higher level...
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update (RequestSamples) state =
  { state: state {samples=Nothing}
  , effects: [ do
      samples <- jsonSamples state.function state.d state.sampleLimit
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
update (DimChange ev) state = 
  -- TODO: maybe some error checking?
  { state: state {d=fromJust $ fromString ev.target.value}
  , effects: [ do
      return RequestSamples
    ]
  }
update (FunctionChange ev) state =
  -- TODO: maybe some error checking?
  { state: state { function=ev.target.value
                 , d=if is3dData ev.target.value then 3 else state.d
                 }
  , effects: [ do
      return RequestSamples
    ]
  }
update (UpdateSampleLimit ev) state =
  case fromString ev.target.value of
       Nothing -> noEffects state
       Just n' -> { state: state {sampleLimit=n'}
                  , effects: [ do
                      return RequestSamples
                    ]
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
        [ dimSelector state
        , funcSelector state.function
        , button [onClick (const RequestSamples)] [text "Fetch samples"]
        ]
    , div [className "sample-controls"]
        [ input [ type_ "range", value (show state.sampleLimit)
                , max "1000", min "0", step "50"
                , onChange UpdateSampleLimit
                ]
                []
        , input [ type_ "text", value (show state.sampleLimit)
                , onChange UpdateSampleLimit] []
        ]
    ]

dimSelector :: State -> Html Action
dimSelector {d=d, function=fn} =
  select [onChange DimChange, value (show d), disabled (is3dData fn)] $
    map (\i -> option [value (show i)] [text $ show i]) (2..10)

funcSelector :: String -> Html Action
funcSelector fname =
  select [onChange FunctionChange, value fname] $
    map (\f -> option [value f] [text f])
      [ "ackley", "rosenbrock", "spherical", "schwefel", "zakharov"
      , "fuel", "rho", "neghip"]

viewError :: Maybe Error -> Html Action
viewError Nothing  = text ""
viewError (Just err) = div [className "error"] [text $ show err]

viewSamples :: State -> Html Action
viewSamples {samples=Nothing} = p [] [text "Nothing loaded"]
viewSamples {overview=Just o} =
  div [className "samples"]
    [ map OverviewView $ Overview.view o ]
    
is3dData :: String -> Boolean
is3dData s = s == "fuel" || s == "rho" || s == "neghip"

