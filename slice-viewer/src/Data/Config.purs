
module Data.Config where

import Prelude
import Data.Maybe (fromMaybe)
import Control.Monad.Eff (Eff)
import Node.Process (PROCESS, lookupEnv)

server :: forall eff. Eff (process :: PROCESS | eff) String
server = do
  devEnv <- fromMaybe "development" <$> lookupEnv "NODE_ENV"
  pure $ if devEnv == "production"
            then "http://sliceplorer.cs.univie.ac.at"
            else "http://localhost:5000"

fullUrl :: forall eff. String -> Eff (process :: PROCESS | eff) String
fullUrl url = do
  baseUrl <- server
  pure $ baseUrl <> "/" <> url

