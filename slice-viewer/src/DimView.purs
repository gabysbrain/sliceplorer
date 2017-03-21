module App.DimView where

import Prelude hiding (div)
import Data.Array (modifyAt, zipWith, concatMap, concat)
import Data.Filterable (filtered)
import Data.Foldable (elem, find, minimum, maximum)
import Data.Int as I
import Data.Maybe
import Data.StrMap as SM
import Data.Tuple (Tuple(..), uncurry)
import Stats (Histogram, histogram, histogram', histRanges)
import Pux.Html (Html, div, text, h3)
import Pux.Html.Attributes (className, key)
--import Pux.Html.Events (onChange, FormEvent)
import Util (mapEnum, mapCombine, zipMap, unsafeJust, numRange, df2a)

import Data.Slices (xLoc, yLoc)
import Data.Samples (combineMaps)

import App.Core (AppData, DimData)
import Data.DataFrame (Query)
import Data.DataFrame as DF
import Data.ValueRange (ValueRange)

import Vis.D3.SliceChart as SV
import Data.SliceSample as Slice

import Debug.Trace

type State =
  { dimName :: String
  , samples :: AppData
  , sliceView :: SV.State
  }

data Action
  = ShowClusterView Boolean
  | FocusPointFilter AppData
  | UpdateSamples AppData
  | SliceViewAction SV.Action

init :: String -> AppData -> Query AppData State
init dn df = do
  yValRange <- functionRange
  let xValRange = DF.runQuery inputRange df
  pure $ { dimName: dn
         , samples: df
         , sliceView: SV.init xValRange yValRange $ df2a df
         }

update :: Action -> State -> State
update (ShowClusterView s) state = state {sliceView = SV.update (SV.ShowClusters s) state.sliceView}
update (UpdateSamples df) state =
  state { samples = df
        , sliceView = SV.update (SV.UpdateSlices $ df2a df) state.sliceView
        }
update (FocusPointFilter fp) state = state
  { sliceView       = SV.update (SV.HighlightSlices $ df2a fp) state.sliceView
  }
  --where 
  --nbrs = findNeighbors state.samples fp
  --neighborSlices = concatMap sample2slice nbrs
update (SliceViewAction a) state =
  state {sliceView=SV.update a state.sliceView}

metricHighlights :: Query AppData (SM.StrMap (Array Number))
metricHighlights = do
  q <- DF.summarize (\(Slice.SliceSample fp) -> fp.metrics)
  pure $ combineMaps q

view :: State -> Html Action
view state =
  div [className "dim-view"]
    [ viewName state.dimName
    , div [className "dim-charts"]
      [ viewAllSlices state
      ]
    ]

viewName :: String -> Html Action
viewName name = div [className "dim-name"] [text name]

viewAllSlices :: State -> Html Action
viewAllSlices state =
  div [className "slices-view"] 
    [ map SliceViewAction $ SV.view state.sliceView ]

--findNeighbors :: AppData -> Array Slice.SliceSample -> Array Slice.SliceSample
--findNeighbors df [(Slice.SliceSample fp)] =
  --DF.rowFilter (neighborFilter fp.neighborIds) df
--findNeighbors _  _       = []

--neighborFilter :: Array Int -> Slice.SliceSample -> Boolean
--neighborFilter fpIds (Slice.SliceSample fp) = elem fp.focusPointId fpIds

functionRange :: Query AppData (Tuple Number Number)
functionRange = do
  yVals <- DF.summarize maxSliceYLoc
  pure $ fromMaybe (Tuple 0.0 0.0) $ numRange (filtered yVals)

maxSliceYLoc :: Slice.SliceSample -> Maybe Number
maxSliceYLoc (Slice.SliceSample s) = maximum $ map yLoc s.slice

inputRange :: Query AppData (Tuple Number Number)
inputRange = do
  xVals <- DF.summarize sliceXLoc
  pure $ fromMaybe (Tuple 0.0 0.0) $ numRange (concat xVals)

sliceXLoc :: Slice.SliceSample -> Array Number
sliceXLoc (Slice.SliceSample s) = map xLoc s.slice

