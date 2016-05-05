module App.FunctionSlices where

import Prelude 
import Data.Maybe (Maybe(Just, Nothing))
import Data.Maybe.Unsafe (fromJust)
import Data.Either (Either(..), either)
import Data.Either.Unsafe (fromRight)
import Data.Array (length, (..), head, slice)
import Data.Int (fromString, toNumber)
import Data.Tuple (fst, snd)
import Data.Foldable (sum, maximum, minimum)
import Math (min)
import Control.Monad.Eff.Exception (Error, error)

import Control.Monad.Aff (attempt)
import Network.HTTP.Affjax (AJAX, get)

import Pux (EffModel, noEffects)
import Pux.Html (Html, a, div, span, button, input, text, p, select, option)
import Pux.Html.Events (onChange, onClick, FormEvent)
import Pux.Html.Attributes (className, selected, value, href)

import Data.Samples (SampleGroup(..), DimSamples(..), Slice(..), parseJson, sort)
import App.Pager as Pager
import App.SampleView as SampleView

data SortKey
  = SliceSort
  | VarianceSort
  | MinValueSort
  | MaxValueSort
  | AvgValueSort
  | AvgGradSort
  | AvgAbsGradSort

data SortAggregation
  = Avg
  | Max
  | Min

type State =
  { d :: Int
  , function :: String
  , sortKey :: SortKey
  , sortAgg :: SortAggregation
  , error :: Maybe Error
  , samples :: Maybe SampleGroup
  , pager :: Pager.State
  }

data Action 
  = RequestSamples 
  | ReceiveSamples (Either Error SampleGroup)
  | DimChange FormEvent
  | FunctionChange FormEvent
  | SortKeyChange FormEvent
  | SortAggChange FormEvent
  | PagerView Pager.Action

init :: State
init = 
  { d: 2
  , function: "spherical"
  , sortKey: SliceSort
  , sortAgg: Avg
  , error: Nothing
  , samples: Nothing
  , pager: Pager.init SampleView.view
  }

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (RequestSamples) state =
  { state: state {samples=Nothing}
  , effects: [ do
      let url = "http://localhost:5000/slice/" ++ state.function ++ "/" ++ (show state.d)
      --res <- attempt $ get "/data/test.csv"
      res <- attempt $ get url
      let samples = case res of
                        Right r  -> parseJson r.response
                        Left err -> Left err
      return $ ReceiveSamples samples
    ]
  }
update (ReceiveSamples (Left err)) state =
  noEffects $ state {error = Just err}
update (ReceiveSamples (Right s)) state =
  let sortedSamples = sampleSort state.sortKey state.sortAgg s
   in { state: state {samples = Just s
                     , error = Nothing
                     }
      , effects: [do
          let decode (SampleGroup xs) = xs
          return $ PagerView (Pager.ChangeChildren (decode sortedSamples))
        ]
      }
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
update (SortKeyChange ev) state | ev.target.value == "Slice" =
  updateSortState SliceSort state.sortAgg state
update (SortKeyChange ev) state | ev.target.value == "Variance" =
  updateSortState VarianceSort state.sortAgg state
update (SortKeyChange ev) state | ev.target.value == "Min Value" =
  updateSortState MinValueSort state.sortAgg state
update (SortKeyChange ev) state | ev.target.value == "Max Value" =
  updateSortState MaxValueSort state.sortAgg state
update (SortKeyChange ev) state | ev.target.value == "Avg Value" =
  updateSortState AvgValueSort state.sortAgg state
update (SortKeyChange ev) state | ev.target.value == "Avg Gradient" =
  updateSortState AvgGradSort state.sortAgg state
update (SortKeyChange ev) state | ev.target.value == "Avg Abs Gradient" =
  updateSortState AvgAbsGradSort state.sortAgg state
update (SortKeyChange ev) state =
  noEffects $ state {error=Just (error (ev.target.value ++ " is not a valid sort key"))}
update (SortAggChange ev) state | ev.target.value == "Average" =
  updateSortState state.sortKey Avg state
update (SortAggChange ev) state | ev.target.value == "Max" =
  updateSortState state.sortKey Max state
update (SortAggChange ev) state | ev.target.value == "Min" =
  updateSortState state.sortKey Min state
update (SortAggChange ev) state =
  noEffects $ state {error=Just (error (ev.target.value ++ " is not a valid sort aggregation"))}
update (PagerView action) state =
  noEffects $ state {pager=Pager.update action state.pager}

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
    , div [className "view-controls"]
        [ metricSorter state.sortKey
        , metricAgg state.sortAgg
        ]
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

metricSorter :: SortKey -> Html Action
metricSorter sortKey = 
  select [onChange SortKeyChange, value sv] $
    map (\f -> option [value f] [text f])
      ["Slice", "Variance", "Min Value", "Max Value", "Avg Value", "Avg Gradient", "Avg Abs Gradient"]
  where
    sv = case sortKey of
              SliceSort -> "Slice"
              VarianceSort -> "Variance"
              MinValueSort -> "Min Value"
              MaxValueSort -> "Max Value"
              AvgValueSort -> "Avg Value"
              AvgGradSort -> "Avg Gradient"
              AvgAbsGradSort -> "Avg Abs Gradient"
  {--radioGroup []--}
    {--[ radio [value "Slice"] []--}
    {--, radio [value "Variance" []--}
    {--]--}

metricAgg :: SortAggregation -> Html Action
metricAgg sortAgg =
  select [onChange SortAggChange, value sa] $
    map (\f -> option [value f] [text f])
      ["Average", "Max", "Min"]
  where
    sa = case sortAgg of
              Avg -> "Average"
              Max -> "Max"
              Min -> "Min"

viewError :: Maybe Error -> Html Action
viewError Nothing  = text ""
viewError (Just err) = div [className "error"] [text $ show err]

viewSamples :: State -> Html Action
viewSamples {samples=Nothing} = p [] [text "Nothing loaded"]
{--viewSamples (Just (SampleGroup s)) mnVal mxVal = --}
  {--div [className "sample-group"] $ map viewSample (slice mnVal mxVal s)--}
  --[ viewSample $ (fromJust <<< head) s ]
viewSamples state@{samples=Just (SampleGroup s)} = 
  div [className "samples"] 
    [ map PagerView $ Pager.view state.pager
    ]
    
updateSortState sortKey sortAgg state = 
  case state.samples of
       Nothing -> noEffects $ state { sortKey=sortKey, sortAgg=sortAgg }
       Just samples -> { state: state { sortKey=sortKey, sortAgg=sortAgg }
                       , effects: [ do
                           return $ ReceiveSamples $ Right samples
                         ]
                       }

sampleSort :: SortKey -> SortAggregation -> SampleGroup -> SampleGroup
--sampleSort key agg Nothing = Nothing
sampleSort key agg = sort (sortFunc key agg)

sortFunc :: SortKey -> SortAggregation -> DimSamples -> DimSamples -> Ordering
sortFunc SliceSort _ (DimSamples {focusPoint=f1}) (DimSamples {focusPoint=f2}) = 
  compare f1 f2
sortFunc VarianceSort agg (DimSamples {slices=s1}) (DimSamples {slices=s2}) =
  compare (aggFunc agg $ map (\(Slice x) -> x.metrics.variance) s2)
          (aggFunc agg $ map (\(Slice x) -> x.metrics.variance) s1) -- this should sort backwards
sortFunc MinValueSort agg (DimSamples {slices=s1}) (DimSamples {slices=s2}) =
  compare (aggFunc agg $ map (\(Slice x) -> x.metrics.minValue) s1)
          (aggFunc agg $ map (\(Slice x) -> x.metrics.minValue) s2)
sortFunc MaxValueSort agg (DimSamples {slices=s1}) (DimSamples {slices=s2}) =
  compare (aggFunc agg $ map (\(Slice x) -> x.metrics.maxValue) s2)
          (aggFunc agg $ map (\(Slice x) -> x.metrics.maxValue) s1) -- this should sort backwards
sortFunc AvgValueSort agg (DimSamples {slices=s1}) (DimSamples {slices=s2}) =
  compare (aggFunc agg $ map (\(Slice x) -> x.metrics.avgValue) s2)
          (aggFunc agg $ map (\(Slice x) -> x.metrics.avgValue) s1) -- this should sort backwards
sortFunc AvgGradSort agg (DimSamples {slices=s1}) (DimSamples {slices=s2}) =
  compare (aggFunc agg $ map (\(Slice x) -> x.metrics.avgGradient) s2)
          (aggFunc agg $ map (\(Slice x) -> x.metrics.avgGradient) s1) -- this should sort backwards
sortFunc AvgAbsGradSort agg (DimSamples {slices=s1}) (DimSamples {slices=s2}) =
  compare (aggFunc agg $ map (\(Slice x) -> x.metrics.avgAbsGradient) s2)
          (aggFunc agg $ map (\(Slice x) -> x.metrics.avgAbsGradient) s1) -- this should sort backwards

aggFunc :: SortAggregation -> Array Number -> Number
aggFunc Avg xs = (sum xs) / (toNumber $ length xs)
aggFunc Max xs = fromJust $ maximum xs
aggFunc Min xs = fromJust $ minimum xs

