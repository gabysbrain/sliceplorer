module App.Overview where

import Prelude hiding (div)
import Data.Array (modifyAt, length)
import Data.Maybe (Maybe(..))
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap as SM
import Data.Foldable (elem, find)
import Data.Int as I
import Pux.Html (Html, div, text, select, option)
import Pux.Html.Attributes (className, value)
import Pux.Html.Events (onChange, FormEvent)
import Util (mapEnum)
import Stats (HistBin)
import App.Core (AppData, GroupData)

import App.SliceSampleView as SSV
import Vis.Vega.Splom as Splom
import Vis.Vega.Slices as SV
import Vis.Vega.Histogram as HV
import App.DimView as DV
import App.GroupView as GV
import App.TopoSpine as TS

import Data.Samples (SampleGroup, dims)
import DataFrame as DF
import Data.SliceSample as Slice

type State = 
  { samples :: AppData
  , datasetName :: String
  , dims :: Int
  , dimViewKey :: Int
  , groupMethod :: GroupMethod
  , focusPointFilter :: AppData
  --, metricRangeFilter :: Maybe MetricRangeFilter
  , sliceSampleView :: SSV.State
  , dimViews :: Array DV.State
  }

data Action 
  = ChangeGroupMethod FormEvent
  | SliceSampleViewAction SSV.Action
  | DimViewAction Int DV.Action

data GroupMethod = GroupByDim | GroupByCluster

instance showGroupMethod :: Show GroupMethod where
  show GroupByDim     = "Dims"
  show GroupByCluster = "Clusters"

init :: String -> SampleGroup -> State
init name sg =
  { samples: df
  , datasetName: name
  , dims: dims sg
  , dimViewKey: length gdf
  , groupMethod: GroupByDim
  , focusPointFilter: DF.filterAll df
  , sliceSampleView: SSV.init (dims sg) df
  , dimViews: map (\{group: d, data: s} -> DV.init df (gn ++ show (I.round d)) s) gdf
  }
  where 
  df = DF.init $ Slice.create sg
  gdf = DF.run $ groupSamples GroupByDim df
  gn = dimViewName GroupByDim

groupByNull :: Slice.SliceSample -> Number
groupByNull = const 0.0

groupByDim :: Slice.SliceSample -> Number
groupByDim (Slice.SliceSample s) = I.toNumber s.d

groupByCluster :: Slice.SliceSample -> Number
groupByCluster (Slice.SliceSample s) = I.toNumber s.clusterId

dimViewName :: GroupMethod -> String
dimViewName GroupByDim = "Dim"
dimViewName GroupByCluster = "Cluster"

filterFocusIds :: Array Int -> Slice.SliceSample -> Boolean
filterFocusIds ids (Slice.SliceSample s) = elem s.focusPointId ids

filterRange :: Int -> String -> HistBin -> Slice.SliceSample -> Boolean
filterRange dim metric hb (Slice.SliceSample s) =
  case SM.lookup metric s.metrics of
       Just v -> v >= hb.start && v <= hb.end
       Nothing -> false

groupSamples :: GroupMethod 
             -> AppData
             -> DF.DataFrame {group :: Number, data :: GroupData}
groupSamples method df = DF.mapRows (clusterGroup method) $ DF.groupBy groupByDim df
  where
  clusterGroup GroupByDim     {group=d,data=df'} = {group:d,data:DF.groupBy groupByNull df'}
  clusterGroup GroupByCluster {group=d,data=df'} = {group:d,data:DF.groupBy groupByCluster df'}

update :: Action -> State -> State
update (ChangeGroupMethod ev) state =
  case ev.target.value of
       "Dims"    -> state { dimViewKey = state.dimViewKey + (length $ initDVs GroupByDim)
                          , groupMethod = GroupByDim
                          , dimViews = initDVs GroupByDim
                          }
       "Clusters" -> state { dimViewKey = state.dimViewKey + (length $ initDVs GroupByCluster)
                           , groupMethod = GroupByCluster
                           , dimViews = initDVs GroupByCluster
                           }
       otherwise -> state
  where
  initDVs gm = mapEnum (\i {group: d, data: s} -> DV.init state.samples ((dimViewName gm) ++ show (I.round d)) s) 
                       (DF.run $ groupSamples gm state.samples)
-- FIXME: see if there's a better way than this deep inspection
update (SliceSampleViewAction a@(SSV.SplomAction (Splom.HoverPoint vp))) state =
  updateFocusPoint (DF.rowFilter (filterFocusIds vp') state.samples) state
  where 
  vp' = map (\x -> I.round $ fromJust $ SM.lookup "id" x) vp
update (SliceSampleViewAction a) state =
  state {sliceSampleView=SSV.update a state.sliceSampleView}
update (DimViewAction dim a@(DV.GroupViewAction _ (GV.SliceViewAction (SV.HoverSlice hs)))) state =
  updateFocusPoint (DF.rowFilter (filterFocusIds hs') state.samples) state
  where 
  hs' = map (\x -> x.slice_id) hs
update (DimViewAction dim a@(DV.GroupViewAction _ (GV.HistoAction metric (HV.HoverBar rng)))) state =
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

updateDimViewFocusPoints :: Array {group :: Int, data :: GroupData} 
                         -> Array DV.State 
                         -> Array DV.State
updateDimViewFocusPoints df dvStates =
  mapEnum updateDV dvStates
  where
  updateDV i state = case find (\{group=g} -> g==i) df of
    Just {data=d} -> DV.update (DV.FocusPointFilter d) state
    Nothing       -> DV.update (DV.FocusPointFilter DF.empty) state

updateFocusPoint :: AppData -> State -> State
updateFocusPoint fp state = state
  { focusPointFilter = fp
  , sliceSampleView = SSV.update (SSV.FocusPointFilter fp) state.sliceSampleView
  , dimViews = updateDimViewFocusPoints groupedViews state.dimViews
  }
  where
  -- need to group the focus points here to be 
  -- passed down to the dim views and such
  groupedViews = map (\s -> s {group=I.round s.group}) $ 
                     DF.run $ groupSamples state.groupMethod fp

view :: State -> Html Action
view state =
  div [] 
    [ div [className "group-controls"] 
        [ select [onChange ChangeGroupMethod, value (show state.groupMethod)] 
            [ option [value (show GroupByDim)]     [text (show GroupByDim)]
            , option [value (show GroupByCluster)] [text (show GroupByCluster)]
            ]
        ]
    , div [className "overview-info"]
        [ map SliceSampleViewAction $ SSV.view state.sliceSampleView
        , TS.view state.datasetName state.dims
        ]
    , viewDims state
    ]

viewDims :: State -> Html Action
viewDims state =
  div [] $ mapEnum initDV state.dimViews
  where
  initDV d s = map (DimViewAction d) $ DV.view s

