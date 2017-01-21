module App.Overview where

import Prelude hiding (div, min, max)
import Data.Array (modifyAt, length, (!!), zipWith)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap as SM
import Data.Foldable (elem, find)
import Data.Int as I
import Pux.Html (Html, div, text, input, select, option)
import Pux.Html.Attributes (className, value, type_, min, max, step)
import Pux.Html.Events (onChange, FormEvent)
import Util (mapEnum)
import Stats (HistBin)
import App.Core (AppData, DimData)

import Vis.Vega.Slices as SV
import Vis.Vega.ClusterSlices as CSV
import Vis.Vega.Histogram as HV
import App.DimView as DV

import Data.Samples (SampleGroup, dimNames, subset)
import Data.Samples as Samples
import DataFrame as DF
import Data.SliceSample as Slice

import Debug.Trace

type State = 
  { samples :: AppData
  , fullGroups :: Array {group :: Number, data :: AppData}
  , datasetName :: String
  , dimNames :: Array String
  , samplesToShow :: Int
  , totalSlices :: Int
  , groupMethod :: GroupMethod
  , focusPointFilter :: AppData
  --, metricRangeFilter :: Maybe MetricRangeFilter
  , dimViews :: Array DV.State
  }

data Action 
  = UpdateNumberFilter FormEvent
  | ChangeGroupMethod FormEvent
  | DimViewAction Int DV.Action

data GroupMethod = GroupByDim | GroupByCluster

instance showGroupMethod :: Show GroupMethod where
  show GroupByDim     = "Dims"
  show GroupByCluster = "Clusters"

init :: String -> SampleGroup -> State
init name sg =
  { samples: df
  , fullGroups: gdf
  , datasetName: name
  , dimNames: dns
  , samplesToShow: 10
  , totalSlices: Samples.length sg
  , groupMethod: GroupByDim
  , focusPointFilter: DF.filterAll df
  , dimViews: initDimViews df dns $ trimGroups 10 gdf
  }
  where 
  df = DF.init $ Slice.create sg
  gdf = DF.run $ DF.groupBy groupByDim df
  dns = dimNames sg

initDimViews :: AppData -> Array String 
             -> Array {group :: Number, data :: AppData} 
             -> Array DV.State
initDimViews fullDf dimNames = map initDimView
  where
  initDimView {group: d, data: s} = DV.init fullDf (dimName dimNames (I.floor d)) s

nDim :: State -> Int
nDim state = length state.dimNames

groupByDim :: Slice.SliceSample -> Number
groupByDim (Slice.SliceSample s) = I.toNumber s.d

filterFocusIds :: Array Int -> Slice.SliceSample -> Boolean
filterFocusIds ids (Slice.SliceSample s) = elem s.focusPointId ids

filterRange :: Int -> String -> HistBin -> Slice.SliceSample -> Boolean
filterRange dim metric hb (Slice.SliceSample s) =
  case SM.lookup metric s.metrics of
       Just v -> v >= hb.start && v <= hb.end
       Nothing -> false

update :: Action -> State -> State
update (UpdateNumberFilter ev) state =
  case I.fromString ev.target.value of
       Nothing -> state
       Just n' | n' <= 0 -> state -- Don't allow invalid numbers
       Just n' -> let trimmedGroups = trimGroups n' state.fullGroups
                      dvUpdate {data:s} dv = DV.update (DV.UpdateSamples s) dv
                   in state { samplesToShow = n' 
                            , dimViews = zipWith dvUpdate trimmedGroups state.dimViews
                            }
update (ChangeGroupMethod ev) state =
  case ev.target.value of
       "Dims"    -> state { groupMethod = GroupByDim
                          , dimViews = map (DV.update (DV.ShowClusterView false)) state.dimViews
                          }
       "Clusters" -> state { groupMethod = GroupByCluster
                           , dimViews = map (DV.update (DV.ShowClusterView true)) state.dimViews
                           }
       otherwise -> state
-- FIXME: see if there's a better way than this deep inspection
update (DimViewAction dim a@(DV.ClusterSliceViewAction (CSV.HoverSlice hs))) state =
  updateFocusPoint (DF.rowFilter (filterFocusIds hs') state.samples) state
  where 
  hs' = map (\x -> x.slice_id) hs
update (DimViewAction dim a@(DV.SliceViewAction (SV.HoverSlice hs))) state =
  updateFocusPoint (DF.rowFilter (filterFocusIds hs') state.samples) state
  where 
  hs' = map (\x -> x.slice_id) hs
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

updateDimViewFocusPoints :: DimData -> Array DV.State -> Array DV.State
updateDimViewFocusPoints dvFp dvStates =
  mapEnum updateDV dvStates
  where
  dvFp' = DF.run dvFp
  updateDV i state = case find (\{group:g} -> g==I.toNumber i) dvFp' of
    Just {data:d} -> DV.update (DV.FocusPointFilter d) state
    Nothing       -> DV.update (DV.FocusPointFilter DF.empty) state

updateFocusPoint :: AppData -> State -> State
updateFocusPoint fp state = state
  { focusPointFilter = fp
  , dimViews = updateDimViewFocusPoints dvFp state.dimViews
  }
  where
  dvFp = DF.groupBy groupByDim fp

view :: State -> Html Action
view state =
  div [] 
    [ div [className "group-controls"] 
        [ select [onChange ChangeGroupMethod, value (show state.groupMethod)] 
            [ option [value (show GroupByDim)]     [text (show GroupByDim)]
            , option [value (show GroupByCluster)] [text (show GroupByCluster)]
            ]
        , input [ type_ "range", value (show state.samplesToShow)
                , max (show state.totalSlices), min "0", step "10"
                , onChange UpdateNumberFilter
                ]
                []
        , input [type_ "text", value (show state.samplesToShow), onChange UpdateNumberFilter] []
        ]
    , viewDims state
    ]

viewDims :: State -> Html Action
viewDims state =
  div [] $ mapEnum initDV state.dimViews
  where
  initDV d s = map (DimViewAction d) $ DV.view s

dimName :: Array String -> Int -> String
dimName dimNames d = fromMaybe ("Dim " <> show d) $ dimNames !! d

trimGroups :: Int -> Array {group :: Number, data :: AppData} 
                  -> Array {group ::Number, data :: AppData}
trimGroups n = map f
  where
  f {group:d,data:dd} = {group: d, data: DF.takeFilter n dd}

