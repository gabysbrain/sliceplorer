module App.DimView where

import Prelude hiding (div)
import Data.Array (modifyAt, zipWith)
import Data.Maybe
import Data.Tuple (Tuple(..))

import Pux.Html (Html, div, text)
import Pux.Html.Attributes (className, key)
--import Pux.Html.Events (onChange, FormEvent)
import Util (mapEnum)

import App.Core (AppData, DimData, GroupData)
import DataFrame as DF

import App.GroupView as GV

type State =
  { dimName :: String
  , samples :: GroupData
  , groupViews :: Array GV.State
  }

data Action =
    GroupViewAction Int GV.Action
  | FocusPointFilter GroupData

init :: AppData -> String -> DimData -> State
init origDf dn df = 
  { dimName: dn
  , samples: df
  , groupViews: mapEnum (\i {group=_,data=df'} -> GV.init origDf i df') $ DF.run df
  }

view :: State -> Html Action
view state =
  div [className "dim-view"]
    [ viewName state.dimName
    , viewGroups state.groupViews
    ]

viewName :: String -> Html Action
viewName name = div [className "dim-name"] [text name]

viewGroups :: Array GV.State -> Html Action
viewGroups []      = div [] []
viewGroups [state] = viewGroup 0 state
viewGroups states  = div [className "group-views"] $
  mapEnum viewGroup states

viewGroup :: Int -> GV.State -> Html Action
viewGroup gid state = map (GroupViewAction gid) $ GV.view state

update :: Action -> State -> State
update (GroupViewAction i a) state =
  updateGroupView i a state
update (FocusPointFilter df) state = state
  { groupViews = zipWith (\{group=_,data=df'} gv -> GV.update (GV.FocusPointFilter df') gv)
                         (DF.run df)
                         state.groupViews
  }

updateGroupView :: Int -> GV.Action -> State -> State
updateGroupView i a state =
  case modifyAt i (GV.update a) state.groupViews of
       Nothing     -> state
       Just newGVs -> state {groupViews=newGVs}

