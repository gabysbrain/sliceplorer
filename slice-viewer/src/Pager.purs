module App.Pager where

import Prelude 
import Data.Array (length, slice)

import App.SampleView as Samples

import Pux.Html (Html, div, button, text)
import Pux.Html.Events (onClick)
import Pux.Html.Attributes (className, disabled)

type State =
  { pageNum     :: Int
  , pageSize    :: Int
  , childRender :: Samples.State -> Html Samples.Action
  , children    :: Array Samples.State
  }

data Action 
  = NextPage
  | PrevPage
  | FirstPage
  | LastPage
  | ChangeChildren (Array Samples.State)
  | ChildAction Samples.Action

init :: (Samples.State -> Html Samples.Action) -> State
init r =
  { pageNum: 0
  , pageSize: 10
  , childRender: r -- this is fixed for now...
  , children: []
  }

min :: Int -> Int -> Int
min x y = if x < y then x else y

max :: Int -> Int -> Int
max x y = if x > y then x else y

maxPageNum :: State -> Int
maxPageNum {pageSize=sz, children=c} = (length c) / sz

update :: Action -> State -> State
update NextPage state =
  state {pageNum=min (state.pageNum+1) (maxPageNum state)}
update PrevPage state =
  state {pageNum=max 0 (state.pageNum-1)}
update FirstPage state =
  state {pageNum=0}
update LastPage state =
  state {pageNum=maxPageNum state}
update (ChangeChildren cs) state =
  state {pageNum=0, children=cs}
update (ChildAction a) state = state -- TODO: change this

view :: State -> Html Action
view state =
  div [] 
    [ viewControls state
    , div [className "page-results"] $ map (map ChildAction <<< state.childRender) renderChildren
    ]
  where 
  startI = state.pageNum*state.pageSize
  endI = min ((state.pageNum+1)*state.pageSize) (length state.children)
  renderChildren = slice startI endI state.children

viewControls :: State -> Html Action
viewControls state =
  div [className "page-controls"] 
    [ button [onClick $ const FirstPage, disabled (state.pageNum==0)] [text "First"]
    , button [onClick $ const PrevPage, disabled (state.pageNum==0)] [text "Prev"]
    , text "Page: "
    , text $ show (state.pageNum+1)
    , text "/"
    , text $ show ((maxPageNum state)+1)
    , button [onClick $ const NextPage, disabled (state.pageNum==(maxPageNum state))] [text "Next"]
    , button [onClick $ const LastPage, disabled (state.pageNum==(maxPageNum state))] [text "Last"]
    ]

