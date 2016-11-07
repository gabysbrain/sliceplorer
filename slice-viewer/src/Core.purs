module App.Core where

import DOM (DOM)
import Network.HTTP.Affjax (AJAX)

import DataFrame (DataFrame)
import Data.SliceSample (SliceSample)


type AppEffects = (dom :: DOM, ajax :: AJAX)
--type AppEffects = (ajax :: AJAX)

type AppData = DataFrame SliceSample
type DimData = DataFrame {group :: Number, data :: AppData}
type GroupData = DataFrame {group :: Number, data :: AppData}

