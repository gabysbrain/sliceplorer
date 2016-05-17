module App.Overview where

import Prelude hiding (div)
import Data.Array (concatMap, snoc, zip, zipWith, concat, length, (!!), (..))
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap as SM
import Data.Int as I
import Pux.Html (Html, div, h3, text, input)
import Pux.Html.Attributes (className, type_, min, max, step, value)
import Pux.Html.Events (onChange, FormEvent)
import Vis.Vega (vegaChart, toVegaData, allSlicesSpec)
import Debug.Trace

import App.DimensionView as DV

import Data.Samples (SampleGroup(..), DimSamples(..), dims, metricHistograms, subset)
import Data.Slices (Slice(..), Sample(..))

type State = 
  { samples :: SampleGroup
  , samplesToShow :: Int
  }

data Action 
  = UpdateNumberFilter FormEvent
  | DimViewAction Int DV.Action

init :: SampleGroup -> State
init sg =
  { samples: sg
  , samplesToShow: 10
  }
  -- FIXME: put all the filters here and filter everything when state changes

update :: Action -> State -> State
update (UpdateNumberFilter ev) state =
  case I.fromString ev.target.value of
       Nothing -> state
       Just n' -> state {samplesToShow=n'}

view :: State -> Html Action
view ({samples=SampleGroup []}) =
  div [] []
view state@{samples=(SampleGroup sg), samplesToShow=n} =
  div [] 
    [ div [className "controls"] 
        [ input [ type_ "range", value (show n)
                , max (show (length sg)), min "0", step "10"
                , onChange UpdateNumberFilter
                ]
                []
        , input [type_ "text", value (show n), onChange UpdateNumberFilter] []
        ]
    , viewDims state
    ]

viewDims :: State -> Html Action
viewDims {samples=sg, samplesToShow=n} =
  div [] $ map initDV (0..((dims sg)-1))
  where
  sg' = subset n sg
  initDV d = map (DimViewAction d) $ DV.view (DV.init d sg')

