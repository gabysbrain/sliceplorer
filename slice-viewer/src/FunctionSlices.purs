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

import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Exception (Error)
import Network.HTTP.Affjax (AJAX, get)

import Pux (EffModel, noEffects)
import Pux.Html (Html, a, div, span, button, input, text, p, select, option)
import Pux.Html.Events (onChange, onClick, FormEvent)
import Pux.Html.Attributes (className, selected, value)

import Data.Samples (SampleGroup(..), DimSamples(..), Samples(..), parse)
import Vis.Vega (vegaChart)

pageSize :: Int
pageSize = 10

type State =
  { pageStart :: Int
  , pageEnd :: Int
  , d :: Int
  , function :: String
  , error :: Maybe Error
  , samples :: Maybe SampleGroup
  }

data Action 
  = RequestSamples 
  | ReceiveSamples (Either Error SampleGroup)
  | DimChange FormEvent
  | FunctionChange FormEvent
  | NextPage
  | PrevPage

init :: State
init = 
  { pageStart: 0
  , pageEnd: 0
  , d: 2
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
  noEffects $ state {samples = Just s, error = Nothing, 
                     pageStart = 0, pageEnd = pageSize}
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
update (NextPage) state =
  noEffects $ state {pageStart=state.pageStart+pageSize, pageEnd=state.pageEnd+pageSize}
update (PrevPage) state =
  noEffects $ state {pageStart=state.pageStart-pageSize, pageEnd=state.pageEnd-pageSize}

view :: State -> Html Action
view state = 
  div []
    [ viewControls state
    , pageControls state.samples
    , viewError state.error
    , viewSamples state.samples state.pageStart state.pageEnd
    ]

viewControls :: State -> Html Action
viewControls state = 
  div [] 
    [ dimSelector state.d
    , funcSelector state.function
    , button [onClick (const RequestSamples)] [text "Fetch samples"]
    ]

pageControls :: Maybe SampleGroup -> Html Action
pageControls Nothing = 
  div [] []
pageControls (Just s) =
  div [] 
    [ a [onClick $ pure PrevPage] [text "Prev"]
    , a [onClick $ pure NextPage] [text "Next"] 
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
viewError (Just err) = span [className "error"] [text $ show err]

viewSamples :: Maybe SampleGroup -> Int -> Int -> Html Action
viewSamples Nothing _ _ = p [] [text "Nothing loaded"]
viewSamples (Just (SampleGroup s)) mnVal mxVal = 
  div [className "sample-group"] $ map viewSample (slice mnVal mxVal s)
  --[ viewSample $ (fromJust <<< head) s ]

viewSample :: DimSamples -> Html Action
viewSample s = div [className "sample"] $ map viewSingleSlice s

viewSingleSlice :: Samples -> Html Action
viewSingleSlice s = vegaChart [className "dim-slice"] jsonSamples
  where
  jsonSamples = map (\x -> {"x": fst x, "y": snd x}) s

