module App.Overview where

import Prelude hiding (div)
import Data.Array (concatMap, modifyAt, snoc, zip, zipWith, concat, length, (!!), (..))
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap as SM
import Data.Int as I
import Pux.Html (Html, div, h3, text, input)
import Pux.Html.Attributes (className, type_, min, max, step, value)
import Pux.Html.Events (onChange, FormEvent)
import Debug.Trace
import Util (mapEnum)

import App.SliceSampleView as SSV
import Vis.Vega.Splom as Splom
import Vis.Vega.Slices as SV
import App.DimensionView as DV

import Data.Samples (SampleGroup(..), FocusPoint(..), MetricRangeFilter, 
                     dims, metricHistograms, subset, getFocusPoint)
import Data.Slices (Slice(..), Sample(..))

type State = 
  { samples :: SampleGroup
  , samplesToShow :: Int
  , focusPointFilter :: Maybe FocusPoint
  , metricRangeFilter :: Maybe MetricRangeFilter
  , sliceSampleView :: SSV.State
  , dimViews :: Array DV.State
  }

data Action 
  = UpdateNumberFilter FormEvent
  | SliceSampleViewAction SSV.Action
  | DimViewAction Int DV.Action

init :: SampleGroup -> State
init sg =
  { samples: sg
  , samplesToShow: 10
  , focusPointFilter: Nothing
  , metricRangeFilter: Nothing
  , sliceSampleView: SSV.init sg'
  , dimViews: map (\d -> DV.init d sg') (0..((dims sg)-1))
  }
  where 
  sg' = subset 10 sg

update :: Action -> State -> State
update (UpdateNumberFilter ev) state =
  case I.fromString ev.target.value of
       Nothing -> state
       Just n' -> 
         let sg' = subset n' state.samples
          in state { samplesToShow = n'
                   , sliceSampleView = SSV.update (SSV.UpdateSamples sg') state.sliceSampleView
                   , dimViews = map (\dv -> DV.update (DV.UpdateSamples sg') dv)
                                    state.dimViews
                   }
-- FIXME: see if there's a better way than this deep inspection
update (SliceSampleViewAction a@(SSV.SplomAction (Splom.HoverPoint vp))) state =
  updateFocusPoint fpf state
  where 
  fpf = map (Splom.focusPoint state.samples) vp
update (SliceSampleViewAction a) state =
  state {sliceSampleView=SSV.update a state.sliceSampleView}
update (DimViewAction dim a@(DV.SliceViewAction (SV.HoverSlice hs))) state =
  updateFocusPoint fpf state
  where
  fpf = map (\s -> fromJust $ getFocusPoint s.slice_id state.samples) hs
update (DimViewAction dim a) state = 
  case modifyAt dim (DV.update a) state.dimViews of
       Nothing -> state
       Just newDVs -> state {dimViews=newDVs}

updateFocusPoint :: Maybe FocusPoint -> State -> State
updateFocusPoint fp state =
  state { focusPointFilter = fp
        , sliceSampleView = SSV.update (SSV.FocusPointFilter fp) state.sliceSampleView
        , dimViews = map (DV.update (DV.FocusPointFilter fp)) state.dimViews
        }

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
    , map SliceSampleViewAction $ SSV.view state.sliceSampleView
    , viewDims state
    ]

viewDims :: State -> Html Action
viewDims state =
  div [] $ mapEnum initDV state.dimViews
  where
  initDV d s = map (DimViewAction d) $ DV.view s

