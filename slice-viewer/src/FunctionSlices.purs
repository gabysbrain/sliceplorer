module App.FunctionSlices where

import Prelude hiding (div, max, min)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(..))
import Data.Array ((..), length, head)
import Data.Int (fromString)
import Control.Monad.Eff.Exception (Error, error)
import Network.HTTP.Affjax (AJAX)

import Pux (EffModel, noEffects)
import Pux.Html (Html, div, button, text, select, option, input)
import Pux.Html.Events (onChange, onClick, FormEvent)
import Pux.Html.Attributes (className, disabled, value, type_, min, max, step)

import Data.Samples (SampleGroup, jsonSamples)
import Data.Dataset (Datasets, Dataset, jsonDatasets, lookup, dsName, dsDims, validDim)
import App.Overview as Overview

import Util (unsafeJust)

type DatasetInfo =
  { datasets :: Datasets
  , function :: String
  , dim :: Int
  , sampleLimit :: Int
  }

-- TODO: rewrite this to be a sum type of the possible data states
data State 
  = LoadingDatasets
  | DatasetLoadingError Error
  | LoadingSamples DatasetInfo
  | SampleLoadingError 
    { dataset :: DatasetInfo
    , error :: Error
    }
  | SamplesLoaded 
    { dataset :: DatasetInfo
    , samples :: SampleGroup
    , overview :: Overview.State
    }

data Action 
  = RequestDatasets
  | UpdateDatasets (Either Error Datasets)
  | RequestSamples 
  | UpdateSamples (Either Error SampleGroup)
  | DimChange FormEvent
  | FunctionChange FormEvent
  | UpdateSampleLimit FormEvent
  | OverviewView Overview.Action

init :: State
init = LoadingDatasets

-- FIXME: ideally unifying this type is handled at a higher level...
update :: forall eff. Action -> State -> EffModel State Action (ajax :: AJAX | eff)
update RequestDatasets state =
  { state: LoadingDatasets
  , effects: [ do
      datasets <- jsonDatasets
      pure $ UpdateDatasets datasets
    ]
  }
update (UpdateDatasets (Left err)) state =
  noEffects $ DatasetLoadingError err
update (UpdateDatasets (Right ds)) state =
  update RequestSamples $ LoadingSamples dsi
  where
  dsi = {datasets: ds, function: "ackley", dim: 2, sampleLimit: 50}
update (RequestSamples) state = case dsInfo state of
  Just dsi -> { state: LoadingSamples dsi
              , effects: [ do
                  samples <- jsonSamples dsi.function dsi.dim dsi.sampleLimit
                  pure $ UpdateSamples samples
                ]
              }
  Nothing -> noEffects state
update (UpdateSamples (Left err)) state = case dsInfo state of
  -- errors cancel everything :(
  Just dsi -> noEffects $ SampleLoadingError {dataset: dsi, error: err}
  Nothing -> noEffects $ DatasetLoadingError err
update (UpdateSamples (Right sg)) state = case dsInfo state of
  Just dsi -> noEffects $ SamplesLoaded { dataset: dsi
                                        , samples: sg
                                        , overview: Overview.init dsi.function sg
                                        }
  Nothing -> noEffects state
update (DimChange ev) state = case dsInfo state of
  -- TODO: maybe some error checking?
  Just dsi -> update RequestSamples (LoadingSamples $ dsi {dim=unsafeJust $ fromString ev.target.value})
  Nothing -> noEffects state
update (FunctionChange ev) state = case dsInfo state of
  -- TODO: maybe some error checking?
  Just dsi -> update RequestSamples $ updateFunction ev.target.value dsi
  Nothing -> noEffects state
update (UpdateSampleLimit ev) state = 
  case dsi' of
       Nothing -> noEffects state
       Just d  -> update RequestSamples (LoadingSamples d)
  where
  dsi' = do
    dsi <- dsInfo state
    n' <- fromString ev.target.value
    pure $ dsi {sampleLimit=n'}
update (OverviewView action) (SamplesLoaded st@{overview:o}) =
  noEffects $ SamplesLoaded (st {overview=Overview.update action o})
update (OverviewView action) state = noEffects state

updateFunction :: String -> DatasetInfo -> State
updateFunction fname dsi = case lookup fname dsi.datasets of
  Nothing -> SampleLoadingError { dataset: dsi
                                , error: error $ "dataset '" <> fname <> "' not found" 
                                }
  Just ds -> LoadingSamples $ dsi { function = fname
                                  , dim = if validDim dsi.dim ds then dsi.dim else unsafeJust $ head $ dsDims ds
                                  }

view :: State -> Html Action
view LoadingDatasets = viewSpinner
view (DatasetLoadingError err) = viewError err
view (LoadingSamples dsi) =
  div []
    [ viewControls dsi
    , viewSpinner
    ]
view (SampleLoadingError {dataset: dsi, error: err}) =
  div []
    [ viewControls dsi
    , viewError err
    ]
view (SamplesLoaded {dataset: dsi, overview: o}) =
  div []
    [ viewControls dsi
    , viewSamples o
    ]

viewControls :: DatasetInfo -> Html Action
viewControls dsi = viewControls' dsi (lookup dsi.function dsi.datasets)

viewControls' :: DatasetInfo -> Maybe Dataset -> Html Action
viewControls' dsi Nothing =
  div []
    [ viewError $ error ("dataset '" <> dsi.function <> "' not found") ]
viewControls' dsi (Just ds) =
  div [] 
    [ div [className "data-controls"] 
        [ dimSelector dsi ds
        , funcSelector dsi ds
        , button [onClick (const RequestSamples)] [text "Fetch samples"]
        ]
    , div [className "sample-controls"]
        [ input [ type_ "range", value (show dsi.sampleLimit)
                , max "1000", min "0", step "50"
                , onChange UpdateSampleLimit
                ]
                []
        , input [ type_ "text", value (show dsi.sampleLimit)
                , onChange UpdateSampleLimit] []
        ]
    ]

dimSelector :: DatasetInfo -> Dataset -> Html Action
dimSelector dsi ds =
  select [onChange DimChange, value (show dsi.dim), disabled (length dims <= 1)] $
    map (\i -> option [value (show i)] [text $ show i]) dims
  where
  dims = dsDims ds

funcSelector :: DatasetInfo -> Dataset -> Html Action
funcSelector dsi ds =
  select [onChange FunctionChange, value dsi.function] $
    map (\f -> option [value f] [text f]) $ map dsName dsi.datasets

viewError :: Error -> Html Action
viewError err = div [className "error"] [text $ show err]

viewSamples :: Overview.State -> Html Action
viewSamples oState =
  div [className "samples"]
    [ map OverviewView $ Overview.view oState ]
    
viewSpinner :: Html Action
viewSpinner = 
  div [className "loading-panel"]
    [ div [className "spinner"] $
        map (\i -> div [className ("spinner-stage-" <> (show i))] []) (1..12)
    , div [className "spinner-text"] [text "Loading..."]
    ]

dsInfo :: State -> Maybe DatasetInfo
dsInfo LoadingDatasets         = Nothing
dsInfo (DatasetLoadingError _) = Nothing
dsInfo (LoadingSamples dsi)    = Just dsi
dsInfo (SampleLoadingError x)  = Just x.dataset
dsInfo (SamplesLoaded x)       = Just x.dataset

