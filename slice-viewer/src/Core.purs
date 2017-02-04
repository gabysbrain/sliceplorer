module App.Core where

import DOM (DOM)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)
import Node.Process (PROCESS)

import Data.DataFrame (DataFrame)
import Data.SliceSample (SliceSample)

type AppEffects = (dom :: DOM, ajax :: AJAX, process :: PROCESS)
--type AppEffects = (ajax :: AJAX)

type AppData = DataFrame SliceSample
type DimInfo = {group :: Tuple Int String, data :: AppData}
type DimData = DataFrame DimInfo

