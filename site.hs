--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import       Data.Monoid ((<>))
import       Hakyll
import       Control.Monad                   (forM_)
import System.FilePath (replaceExtension, replaceDirectory, takeFileName, takeBaseName)
import Data.List.Split (splitOneOf)
import Data.Strings (strStartsWith)
import           Data.Typeable (Typeable)
import           Data.Binary                   (Binary)

import Debug.Trace

--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync -avr --delete _site/ torsnet6cs@sliceplorer.cs.univie.ac.at:evaluation_site/"
  }

main :: IO ()
main = hakyllWith config $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  {-match "images/*_*.png" $ version "exImg" $ do-}
    {-route idRoute-}
    {-compile getResourceFilePath-}

  match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler

  match "css/*.hs" $ do
    route $ setExtension "css"
    compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

  match "examples/*.md" $ do
    route $ setExtension "html"
    compile $ do
      --let images = (fmap.map) (flip Item ()) $ getMatches $ (.&&.) hasNoVersion $ fromGlob "images/*_*.png"
          --ctx = listField "imgs" exCtx images <> defaultContext
      --exImgs <- getMatches $ fromGlob "images/*.png"
      exImgs <- loadAll "images/*.png"
      {-let ctx = listField "exImages" exImgCtx (return exImgs) <>-}
                {-exCtx-}
      pandocCompiler
        >>= loadAndApplyTemplate "templates/task_example.html" (exCtx exImgs)
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      getResourceBody
        >>= applyAsTemplate defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
exCtx :: [Item CopyFile] -> Context String
exCtx exImgs = 
  imgListField "exImages" exImgCtx exImgs <>
  taskField      "task"      <>
  techniqueField "technique" <>
  imgField       "imgUrl"    <>
  defaultContext

exImgCtx :: Context CopyFile
exImgCtx =
  urlField "url" <>
  datasetInfoCtx

datasetInfoCtx :: Context a
datasetInfoCtx = Context $ \f a i ->
  let (Context c) = datasetInfo $ imgDataCode i
  in c f a i

imgField :: String -> Context String
imgField fld = field fld $ 
  return . itemImgPath

imgListField :: String -> Context CopyFile -> [Item CopyFile] -> Context a
imgListField fld ctx imgs = 
  listFieldWith fld ctx $ \i -> do
    return $ itemImages i imgs

taskField :: String -> Context String
taskField fld = field fld $ 
  return . humanizeTaskCode . itemTaskCode

techniqueField :: String -> Context String
techniqueField fld = field fld $
  return . humanizeTechniqueCode . itemTechniqueCode

itemTaskCode = itemCode 0

itemTechniqueCode = itemCode 1

itemTaskTechniqueCode i =
  itemTaskCode i <> "_" <> itemTechniqueCode i

itemImages i = filter cmp
  where
  cmp = (`strStartsWith` itemTechniqueCode i) 
      . takeBaseName . toFilePath . itemIdentifier

itemImgPath = 
  (`replaceDirectory` "/images/") . (`replaceExtension` "pdf") .
  toFilePath . itemIdentifier

imgTechniqueCode = itemCode 0

imgDataCode = itemCode 1

itemCode pos =
  (!! pos) . splitOneOf "_." . takeFileName . toFilePath . itemIdentifier

humanizeTaskCode :: String -> String
humanizeTaskCode "anomaly"      = "Find anomalies"
humanizeTaskCode "cluster"      = "Cluster"
humanizeTaskCode "correlate"    = "Correlate"
humanizeTaskCode "derive"       = "Derive"
humanizeTaskCode "distribution" = "Distribution"
humanizeTaskCode "extremum"     = "Find extrema"
humanizeTaskCode "filter"       = "Filter"
humanizeTaskCode "lookup"       = "Lookup"
humanizeTaskCode "range"        = "Range"
humanizeTaskCode code           = fail $ "Unknown task code: " <> code

humanizeTechniqueCode :: String -> String
humanizeTechniqueCode "ct" = "Contour tree"
humanizeTechniqueCode "hs" = "HyperSlice"
humanizeTechniqueCode "ms" = "Morse-smale complex (Gerber et al.)"
humanizeTechniqueCode "sp" = "1D slices"
humanizeTechniqueCode "ts" = "Topological spine"
humanizeTechniqueCode code = fail $ "Unknown technique code: " <> code

datasetInfo :: String -> Context a
datasetInfo "sinc2d" =
  constField "name" "Sinc function" <>
  constField "dims" "2"
datasetInfo "ackley6d" =
  constField "name" "Ackley function" <>
  constField "dims" "6"
datasetInfo "rosenbrock" =
  constField "name" "Rosenbrock function" <>
  constField "dims" "???"
datasetInfo "borehole" =
  constField "name" "borehole" <>
  constField "dims" "8"
datasetInfo "boston-svm" =
  constField "name" "SVM w/ radial basis on Boston housing dataset" <>
  constField "dims" "13"
datasetInfo "boston-nn" =
  constField "name" "Neural network w/ 26 node hidden layer on Boston housing dataset" <>
  constField "dims" "13"
datasetInfo "fuel" =
  constField "name" "Fuel dataset" <>
  constField "dims" "3"
datasetInfo "neghip" =
  constField "name" "Neghip dataset" <>
  constField "dims" "3"
datasetInfo code = mempty
--fail $ "Unknown dataset code: " <> code

{-data Task = Lookup-}

{-data Technique -}
  {-= Sliceplorer -}
  {-| Hyperslice-}
  {--- | TopoSpines-}

{-data Ex = Ex-}
  {-{ task :: Task-}
  {-, technique :: Technique-}
  {-, dataset :: String-}
  {-, dims :: Int-}
  {-, numSamples :: Int-}
  {-, samplingMethod :: String-}
  {-, neighborhoodMethod :: String-}
  {-, otherParams :: String-}
  {-}-}

{-taskExamples =-}
  {-[ Ex Lookup Sliceplorer "Ackley" 2 50  "Sobol" "N/A" ""-}
  {-, Ex Lookup Hyperslice  "Ackley" 2 150 "Sobol" "N/A" ""-}
  {-]-}

{-exUrl :: Ex -> String-}
{-exUrl ex = "/examples/" <> taskId ex <> "/" <> techniqueId ex <> ".html"-}

{-exImgUrl :: Ex -> String-}
{-exImgUrl ex = "/images/" <> taskId ex <> "/" <> techniqueId ex <> ".pdf"-}

{-taskId :: Ex -> String-}
{-taskId ex = case task ex of-}
    {-Lookup -> "lookup"-}

{-techniqueId :: Ex -> String-}
{-techniqueId ex = case technique ex of-}
  {-Sliceplorer -> "sliceplorer"-}
  {-Hyperslice -> "hs"-}

