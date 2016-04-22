module App.FunctionSlices where

import Prelude 
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(..), either)
import Data.Either.Unsafe (fromRight)
import Data.Array (length)

import Control.Monad.Aff (attempt)
import Network.HTTP.Affjax (AJAX, get)

import Pux (EffModel, noEffects)
import Pux.Html (Html, div, button, input, text, p)
import Pux.Html.Events (onChange, onClick)

import Data.Samples (SampleGroup(..), parse)

type State =
  { d :: Int
  , function :: String
  , errorMsg :: String
  , samples :: Maybe SampleGroup
  }

data Action = RequestSamples | ReceiveSamples SampleGroup

init :: State
init = 
  { d: 2
  , function: "spherical"
  , errorMsg: ""
  , samples: Nothing
  }

update :: Action -> State -> EffModel State Action (ajax :: AJAX)
update (RequestSamples) state =
  { state: state {samples=Nothing}
  , effects: [ do
      --res <- attempt $ get "/data/spherical_2_slices.csv"
      res <- attempt $ get "/data/test.csv"
      let x = fromRight res
      let samples = parse x.response
      return $ ReceiveSamples samples
    ]
  }
update (ReceiveSamples s) state =
  noEffects $ state {samples = Just s, errorMsg = "test"}

view :: State -> Html Action
view state = 
  div []
    [ button [onClick (const RequestSamples)] [text "Fetch samples"]
    , p [] [ case state.samples of
               Nothing -> text "Nothing loaded"
               Just (SampleGroup s)  -> text (show (length s)) ]
    ]

