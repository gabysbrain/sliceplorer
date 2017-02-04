
module Data.Dataset where

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either(..))
import Data.Foldable (find, elem)
import Data.Foreign.Class (class IsForeign, readProp, readJSON)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (runExcept)
import Network.HTTP.Affjax (AJAX, get)
import Node.Process (PROCESS)

import Data.Config (fullUrl)

type Datasets = Array Dataset

data Dataset = Dataset
  { name :: String
  , dims :: Array Int
  }

instance datasetIsForeign :: IsForeign Dataset where
  read json = do
    n <- readProp "dataset" json
    d <- readProp "dims" json
    pure $ Dataset {name: n, dims: d}

jsonDatasets :: forall eff. Aff (ajax :: AJAX, process :: PROCESS | eff) (Either Error Datasets)
jsonDatasets = do
  url <- liftEff $ fullUrl "/slice"
  res <- attempt $ get url
  let datasets = case res of
        Right r -> parseJson r.response
        Left err -> Left err
  pure datasets

parseJson :: String -> Either Error Datasets
parseJson json = case runExcept $ readJSON json of
                      Left err -> Left (error $ show err)
                      Right res -> Right res

lookup :: String -> Datasets -> Maybe Dataset
lookup ds datasets = find (\(Dataset d) -> d.name == ds) datasets

dsName :: Dataset -> String
dsName (Dataset ds) = ds.name

dsDims :: Dataset -> Array Int
dsDims (Dataset ds) = ds.dims

validDim :: Int -> Dataset -> Boolean
validDim d (Dataset ds) = elem d ds.dims

