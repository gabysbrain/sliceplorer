module App.Overview where

import Prelude hiding (div, min, max)
import Data.Array (modifyAt, length, (!!), zipWith)
import Data.DataFrame (Query)
import Data.DataFrame as DF
import Data.Foldable (elem, find, any)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.StrMap as SM
import Data.Tuple (Tuple(..))
import Data.Int as I
import Pux.Html (Html, div, text, input, select, option)
import Pux.Html.Attributes (className, value, type_, min, max, step)
import Pux.Html.Events (onChange, FormEvent)
import Util (mapEnum)
import Stats (HistBin)
import App.Core (AppData, DimData)

import Vis.D3.SliceChart as SV
import Vis.D3.Histogram as HV
import App.DimView as DV

import Data.Samples (SampleGroup, dimNames, subset)
import Data.Samples as Samples
import Data.SliceSample as Slice

import Debug.Trace

type State = 
  { samples :: AppData
  , clickedSamples :: AppData
  , datasetName :: String
  , dimNames :: Array String
  , samplesToShow :: Int
  , totalSlices :: Int
  , groupMethod :: GroupMethod
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
  , clickedSamples: mempty
  , datasetName: name
  , dimNames: dimNames sg
  , samplesToShow: 50
  , totalSlices: Samples.length sg
  , groupMethod: GroupByDim
  , dimViews: DF.runQuery (initDimViews 50) df
  }
  where 
  df = DF.init $ Slice.create sg

initDimViews :: Int -> Query AppData (Array DV.State)
initDimViews n = do
  df <- DF.reset
  q <- trimmedGroups n `DF.chain` initDimViews'
  pure $ map (flip DF.runQuery df) q

initDimViews' :: Query DimData (Array (Query AppData DV.State))
initDimViews' = DF.summarize initDimView

initDimView :: {group :: Tuple Int String, data :: AppData} -> Query AppData DV.State
initDimView {group: Tuple _ dn, data: s} = DV.init dn s

trimmedGroups :: Int -> Query AppData DimData
trimmedGroups n = groupByDim `DF.chain` trimDimData n

trimDimData :: Int -> Query DimData DimData
trimDimData n = DF.mutate trim'
  where
  trim' dd@{data: s} = dd {data=DF.runQuery (DF.trim n) s}

nDim :: State -> Int
nDim state = length state.dimNames

groupByDim :: Query AppData DimData
groupByDim = DF.group dimInfo `DF.chain` DF.sort ord
  where
  dimInfo (Slice.SliceSample s) = Tuple s.d s.dimName
  ord {group:(Tuple d1 _)} {group:(Tuple d2 _)} = compare d1 d2

filterAll :: Query AppData AppData
filterAll = DF.filter $ const false

filterFocusIds :: Array Int -> Query AppData AppData
filterFocusIds ids = DF.filter (filterFocusIds' ids)

filterNonFocusIds :: Array Int -> Query AppData AppData
filterNonFocusIds ids = DF.filter (not <<< filterFocusIds' ids)

filterFocusIds' :: Array Int -> Slice.SliceSample -> Boolean
filterFocusIds' ids (Slice.SliceSample s) = elem s.focusPointId ids

filterRange :: Int -> String -> HistBin -> Query AppData AppData
filterRange dim metric hb = DF.filter (filterRange' dim metric hb)

filterRange' :: Int -> String -> HistBin -> Slice.SliceSample -> Boolean
filterRange' dim metric hb (Slice.SliceSample s) =
  case SM.lookup metric s.metrics of
       Just v -> v >= hb.start && v <= hb.end
       Nothing -> false

symDiff :: AppData -> AppData -> AppData
symDiff d1 d2 | DF.rows d1 == 0 && DF.rows d2 == 0 = mempty
              | DF.rows d1 == 0 = d2
              | DF.rows d2 == 0 = d1
              | otherwise  = d1' <> d2'
  where 
  d1' = spy $ DF.runQuery (DF.filter (\s -> elemBy Slice.fpEq d2 s)) d1
  d2' = spy $ DF.runQuery (DF.filter (\s -> elemBy Slice.fpEq d1 s)) d2

elemBy :: (Slice.SliceSample -> Slice.SliceSample -> Boolean) 
       -> AppData
       -> Slice.SliceSample 
       -> Boolean
elemBy p d s = any (p s) d

update :: Action -> State -> State
update (UpdateNumberFilter ev) state =
  case I.fromString ev.target.value of
       Nothing -> state
       Just n' | n' <= 0 -> state -- Don't allow invalid numbers
       Just n' -> let tg = DF.runQuery (trimmedGroups n' `DF.chain` DF.summarize id) state.samples
                      dvUpdate {data:s} dv = DV.update (DV.UpdateSamples s) dv
                   in state { samplesToShow = n' 
                            , dimViews = zipWith dvUpdate tg state.dimViews
                            }
update (ChangeGroupMethod ev) state =
  case ev.target.value of
       "Dims"     -> state { groupMethod = GroupByDim
                           , dimViews = map (DV.update (DV.ShowClusterView false)) state.dimViews
                           }
       "Clusters" -> state { groupMethod = GroupByCluster
                           , dimViews = map (DV.update (DV.ShowClusterView true)) state.dimViews
                           }
       otherwise -> state
-- FIXME: see if there's a better way than this deep inspection
update (DimViewAction dim a@(DV.SliceViewAction (SV.HoverSlice hs))) state =
  updateFocusPoint (DF.runQuery (filterFocusIds hs') state.samples) state
  where 
  hs' = map Slice.focusPointId hs
update (DimViewAction dim a@(DV.HistoAction metric (HV.HoverBar rng))) state =
  updateDimView dim a $ updateFocusPoint df state -- maintain bar highlight
  where
  df = case rng of
            Just r -> DF.runQuery (filterRange dim metric r) state.samples
            Nothing -> DF.runQuery filterAll state.samples
update (DimViewAction dim a@(DV.SliceViewAction (SV.ClickSlice hs))) state =
  updateFocusPoint mempty state'
  where 
  hs' = map Slice.focusPointId hs
  clickedSlices = DF.runQuery (filterFocusIds hs') state.samples
  state' = spy $ state { clickedSamples = symDiff clickedSlices state.clickedSamples }
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
  updateDV i state = case find (\{group:Tuple g _} -> g==i) dvFp of
    Just {data:d} -> DV.update (DV.FocusPointFilter d) state
    Nothing       -> DV.update (DV.FocusPointFilter mempty) state

updateFocusPoint :: AppData -> State -> State
updateFocusPoint hoverFps state = state
  { dimViews = updateDimViewFocusPoints dvFp state.dimViews }
  where
  dvFp = DF.runQuery groupByDim (state.clickedSamples <> hoverFps)

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

