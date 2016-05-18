module App.FunctionSlices where

import Prelude 
import Data.Maybe (Maybe(Just, Nothing))
import Data.Maybe.Unsafe (fromJust)
import Data.Either (Either(..), either)
import Data.Either.Unsafe (fromRight)
import Data.Array (length, (..), head, slice)
import Data.Int (fromString, toNumber)
import Data.Tuple (fst, snd)
import Data.StrMap (lookup)
import Data.Foldable (sum, maximum, minimum)
import Math (min)
import Control.Monad.Eff.Exception (Error, error)
import Network.HTTP.Affjax (AJAX)

import Pux (EffModel, noEffects)
import Pux.Html (Html, a, div, span, button, input, text, p, select, option)
import Pux.Html.Events (onChange, onClick, FormEvent)
import Pux.Html.Attributes (className, selected, value, href)

import Data.Samples (SampleGroup(..), DimSamples(..),
                     jsonSamples, sortBy, metricNames)
import Data.Slices (Slice(..))
import App.Pager as Pager
import App.SampleView as SampleView
import App.Overview as Overview

{--data SortKey--}
  {--= SliceSort--}
  {--| MetricSort String--}

{--data SortAggregation--}
  {--= Avg--}
  {--| Max--}
  {--| Min--}

type State =
  { d :: Int
  , function :: String
  {--, sortKey :: SortKey--}
  {--, sortAgg :: SortAggregation--}
  , sliceMetrics :: Array String
  , error :: Maybe Error
  , samples :: Maybe SampleGroup
  --, pager :: Pager.State
  , overview :: Maybe Overview.State
  }

data Action 
  = RequestSamples 
  | UpdateSamples (Either Error SampleGroup)
  | DimChange FormEvent
  | FunctionChange FormEvent
  {--| SortKeyChange FormEvent--}
  {--| SortAggChange FormEvent--}
  {--| PagerView Pager.Action--}
  | OverviewView Overview.Action

init :: State
init = 
  { d: 2
  , function: "spherical"
  {--, sortKey: SliceSort--}
  {--, sortAgg: Avg--}
  , sliceMetrics: []
  , error: Nothing
  , samples: Nothing
  --, pager: Pager.init SampleView.view
  , overview: Nothing
  }

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (RequestSamples) state =
  { state: state {samples=Nothing}
  , effects: [ do
      samples <- jsonSamples state.function state.d
      return $ UpdateSamples samples
    ]
  }
update (UpdateSamples (Left err)) state =
  noEffects $ state {error = Just err}
update (UpdateSamples (Right s@(SampleGroup xs))) state =
  noEffects $ state { samples = Just s
                    , sliceMetrics = metricNames (fromJust $ head xs)
                    , overview = Just (Overview.init s)
                    , error = Nothing
                    }
  {--let sortedSamples = sampleSort state.sortKey state.sortAgg s--}
   {--in { state: state { samples = Just s--}
                     {--, sliceMetrics = metricNames (fromJust $ head xs)--}
                     {--, error = Nothing--}
                     {--}--}
      {--, effects: [do--}
          {--let decode (SampleGroup xs') = xs'--}
          {--return $ PagerView (Pager.ChangeChildren (decode sortedSamples))--}
        {--]--}
      {--}--}
update (DimChange ev) state = 
  -- TODO: maybe some error checking?
  { state: state {d=fromJust $ fromString ev.target.value}
  , effects: [ do
      return RequestSamples
    ]
  }
update (FunctionChange ev) state = 
  -- TODO: maybe some error checking?
  { state: state {function=ev.target.value}
  , effects: [ do
      return RequestSamples
    ]
  }
{--update (SortKeyChange ev) state | ev.target.value == "Slice" =--}
  {--updateSortState SliceSort state.sortAgg state--}
{--update (SortKeyChange ev) state =--}
  {--updateSortState (MetricSort ev.target.value) state.sortAgg state--}
{--update (SortAggChange ev) state | ev.target.value == "Average" =--}
  {--updateSortState state.sortKey Avg state--}
{--update (SortAggChange ev) state | ev.target.value == "Max" =--}
  {--updateSortState state.sortKey Max state--}
{--update (SortAggChange ev) state | ev.target.value == "Min" =--}
  {--updateSortState state.sortKey Min state--}
{--update (SortAggChange ev) state =--}
  {--noEffects $ state {error=Just (error (ev.target.value ++ " is not a valid sort aggregation"))}--}
{--update (PagerView action) state =--}
  {--noEffects $ state {pager=Pager.update action state.pager}--}
update (OverviewView action) state =
  noEffects $ case state.overview of
                   Nothing -> state
                   Just o -> state {overview=Just (Overview.update action o)}

view :: State -> Html Action
view state = 
  div []
    [ viewControls state
    , viewError state.error
    , viewSamples state
    ]

viewControls :: State -> Html Action
viewControls state = 
  div [] 
    [ div [className "data-controls"] 
        [ dimSelector state.d
        , funcSelector state.function
        , button [onClick (const RequestSamples)] [text "Fetch samples"]
        ]
    {--, div [className "view-controls"]--}
        {--[ metricSorter state--}
        {--, metricAgg state.sortAgg--}
        {--]--}
    ]

dimSelector :: Int -> Html Action
dimSelector d =
  select [onChange DimChange, value (show d)] $
    map (\i -> option [value (show i)] [text $ show i]) (2..10)

funcSelector :: String -> Html Action
funcSelector fname =
  select [onChange FunctionChange, value fname] $
    map (\f -> option [value f] [text f])
      ["ackley", "rosenbrock", "spherical", "schwefel", "zakharov"]

{--metricSorter :: State -> Html Action--}
{--metricSorter state = --}
  {--select [onChange SortKeyChange, value sv] $--}
    {--map (\f -> option [value f] [text f]) (["Slice"] ++ state.sliceMetrics)--}
  {--where--}
    {--sv = case state.sortKey of--}
              {--SliceSort -> "Slice"--}
              {--MetricSort v -> v--}

{--metricAgg :: SortAggregation -> Html Action--}
{--metricAgg sortAgg =--}
  {--select [onChange SortAggChange, value sa] $--}
    {--map (\f -> option [value f] [text f])--}
      {--["Average", "Max", "Min"]--}
  {--where--}
    {--sa = case sortAgg of--}
              {--Avg -> "Average"--}
              {--Max -> "Max"--}
              {--Min -> "Min"--}

viewError :: Maybe Error -> Html Action
viewError Nothing  = text ""
viewError (Just err) = div [className "error"] [text $ show err]

viewSamples :: State -> Html Action
viewSamples {samples=Nothing} = p [] [text "Nothing loaded"]
{--viewSamples state@{samples=Just (SampleGroup s)} = --}
  {--div [className "samples"] --}
    {--[ map PagerView $ Pager.view state.pager--}
    {--]--}
viewSamples {overview=Just o} =
  div [className "samples"]
    [ map OverviewView $ Overview.view o ]
    
{--updateSortState sortKey sortAgg state = --}
  {--case state.samples of--}
       {--Nothing -> noEffects $ state { sortKey=sortKey, sortAgg=sortAgg }--}
       {--Just samples -> { state: state { sortKey=sortKey, sortAgg=sortAgg }--}
                       {--, effects: [ do--}
                           {--return $ UpdateSamples $ Right samples--}
                         {--]--}
                       {--}--}

{--sampleSort :: SortKey -> SortAggregation -> SampleGroup -> SampleGroup--}
{--sampleSort key agg = sortBy (sortFunc key agg)--}

{--sortFunc :: SortKey -> SortAggregation -> DimSamples -> DimSamples -> Ordering--}
{--sortFunc SliceSort _ (DimSamples {focusPoint=f1}) (DimSamples {focusPoint=f2}) = --}
  {--compare f1 f2--}
{--sortFunc (MetricSort k) agg (DimSamples {slices=s1}) (DimSamples {slices=s2}) =--}
  {--compare (aggFunc agg $ sliceMetrics s2)--}
          {--(aggFunc agg $ sliceMetrics s1) -- this should sort backwards--}
  {--where sliceMetrics = map (\(Slice x) -> fromJust $ lookup k x.metrics)--}

{--aggFunc :: SortAggregation -> Array Number -> Number--}
{--aggFunc Avg xs = (sum xs) / (toNumber $ length xs)--}
{--aggFunc Max xs = fromJust $ maximum xs--}
{--aggFunc Min xs = fromJust $ minimum xs--}

