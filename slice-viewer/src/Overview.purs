module App.Overview where

import Prelude hiding (div)
import Data.Array (concatMap, modifyAt, snoc, zip, zipWith, concat, length, (!!), (..))
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap as SM
import Data.Foldable (elem)
import Data.Int as I
import Pux.Html (Html, div, h3, text, input)
import Pux.Html.Attributes (className, type_, min, max, step, value)
import Pux.Html.Events (onChange, FormEvent)
import Util (mapEnum)
import Stats (HistBin)

import App.SliceSampleView as SSV
import Vis.Vega.Splom as Splom
import Vis.Vega.Slices as SV
import Vis.Vega.Histogram as HV
import App.DimensionView as DV

import Data.Samples (SampleGroup, dims)
import DataFrame as DF
import Data.SliceSample as Slice

type State = 
  { samples :: DF.DataFrame Slice.SliceSample
  , dims :: Int
  , focusPointFilter :: DF.DataFrame Slice.SliceSample
  --, metricRangeFilter :: Maybe MetricRangeFilter
  , sliceSampleView :: SSV.State
  , dimViews :: Array DV.State
  }

data Action 
  = SliceSampleViewAction SSV.Action
  | DimViewAction Int DV.Action

init :: SampleGroup -> State
init sg =
  { samples: df
  , dims: dims sg
  , focusPointFilter: DF.filterAll df
  , sliceSampleView: SSV.init (dims sg) df
  , dimViews: map (\{group: d, data: s} -> DV.init (I.round d) s) 
                  (DF.run $ DF.groupBy groupByDim df)
  }
  where 
  df = DF.init $ Slice.create sg

groupByDim :: Slice.SliceSample -> Number
groupByDim (Slice.SliceSample s) = I.toNumber s.d

filterFocusIds :: Array Int -> Slice.SliceSample -> Boolean
filterFocusIds ids (Slice.SliceSample s) = elem s.focusPointId ids

filterRange :: Int -> String -> HistBin -> Slice.SliceSample -> Boolean
filterRange dim metric hb (Slice.SliceSample s) =
  case SM.lookup metric s.metrics of
       Just v -> dim == s.d && v >= hb.start && v <= hb.end
       Nothing -> false

update :: Action -> State -> State
-- FIXME: see if there's a better way than this deep inspection
update (SliceSampleViewAction a@(SSV.SplomAction (Splom.HoverPoint vp))) state =
  updateFocusPoint (DF.rowFilter (filterFocusIds vp) state.samples) state
update (SliceSampleViewAction a) state =
  state {sliceSampleView=SSV.update a state.sliceSampleView}
update (DimViewAction dim a@(DV.SliceViewAction (SV.HoverSlice hs))) state =
  updateFocusPoint (DF.rowFilter (filterFocusIds hs) state.samples) state
update (DimViewAction dim a@(DV.HistoAction metric (HV.HoverBar rng))) state =
  updateDimView dim a $ updateFocusPoint df state -- maintain bar highlight
  where
  df = case rng of
            Just r -> DF.rowFilter (filterRange dim metric r) state.samples
            Nothing -> DF.filterAll state.samples
update (DimViewAction dim a) state = 
  updateDimView dim a state

updateDimView :: Int -> DV.Action -> State -> State
updateDimView dim a state =
  case modifyAt dim (DV.update a) state.dimViews of
       Nothing -> state
       Just newDVs -> state {dimViews=newDVs}

updateFocusPoint :: DF.DataFrame Slice.SliceSample -> State -> State
updateFocusPoint fp state = state 
  { focusPointFilter = fp
  , sliceSampleView = SSV.update (SSV.FocusPointFilter fpIds) state.sliceSampleView
  , dimViews = map (DV.update (DV.FocusPointFilter fp)) state.dimViews
  }
  where
  fpIds = map (\(Slice.SliceSample s) -> s.focusPointId) $ DF.run fp

view :: State -> Html Action
view state =
  div [] 
    [ map SliceSampleViewAction $ SSV.view state.sliceSampleView
    , viewDims state
    ]

viewDims :: State -> Html Action
viewDims state =
  div [] $ mapEnum initDV state.dimViews
  where
  initDV d s = map (DimViewAction d) $ DV.view s

