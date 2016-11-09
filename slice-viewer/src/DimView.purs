module App.DimView where

import Prelude hiding (div)
import Data.Array (modifyAt, zipWith)
import Data.Foldable (elem, find)
import Data.Int as I
import Data.Maybe
import Data.Tuple (Tuple(..))

import Pux.Html (Html, div, text)
import Pux.Html.Attributes (className, key)
--import Pux.Html.Events (onChange, FormEvent)
import Util (mapEnum)

import App.Core (AppData, DimData, GroupData)
import DataFrame as DF

import App.GroupView as GV
import Data.SliceSample as Slice

type State =
  { dimName :: String
  --, samples :: AppData
  , showClusters :: Boolean
  , fullView :: GV.State
  , clusterViews :: Array GV.State
  }

data Action =
    GroupViewAction Int GV.Action
  | ShowClusterView Boolean
  | FocusPointFilter AppData

init :: AppData -> String -> AppData -> State
init origDf dn df = 
  { dimName: dn
  --, samples: df
  , showClusters: false
  , fullView: GV.init origDf 0 df
  , clusterViews: map (\{group=g,data=df'} -> GV.init origDf (I.round g) df')
                      (DF.run $ clusterGroups df)
  }

clusterGroups :: AppData -> GroupData
clusterGroups = DF.groupBy groupByCluster
  where
  groupByCluster (Slice.SliceSample s) = I.toNumber s.clusterId

view :: State -> Html Action
view state =
  div [className "dim-view"]
    [ viewName state.dimName
    , viewGroups state.showClusters state
    ]

viewName :: String -> Html Action
viewName name = div [className "dim-name"] [text name]

viewGroups :: Boolean -> State -> Html Action
viewGroups false state = viewGroup 0 state.fullView
viewGroups true  state = div [className "group-views"] $
  mapEnum viewGroup state.clusterViews

viewGroup :: Int -> GV.State -> Html Action
viewGroup gid state = map (GroupViewAction gid) $ GV.view state

update :: Action -> State -> State
update (GroupViewAction i a) state =
  updateGroupView i a state
update (ShowClusterView s) state = state {showClusters=s}
update (FocusPointFilter fp) state = state
  { fullView = GV.update (GV.FocusPointFilter fp) state.fullView
  , clusterViews = map updateFp state.clusterViews
  }
  where
  groupedFp = DF.run $ clusterGroups fp
  updateFp gvs = case find (\{group=g} -> g==I.toNumber gvs.key) groupedFp of
    Just {data=d} -> GV.update (GV.FocusPointFilter d) gvs
    Nothing       -> GV.update (GV.FocusPointFilter DF.empty) gvs

updateGroupView :: Int -> GV.Action -> State -> State
updateGroupView i a state =
  case modifyAt i (GV.update a) state.clusterViews of
       Nothing     -> state
       Just newGVs -> state {clusterViews=newGVs}

