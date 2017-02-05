
module Data.Config where

import Prelude
import Data.Maybe (fromMaybe)
import Data.String (take)
import Control.Monad.Eff (Eff)

foreign import server :: String

fullUrl :: String -> String
fullUrl url | take 1 url == "/" = server <> url
fullUrl url                     = server <> "/" <> url

