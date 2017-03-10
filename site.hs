--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
import       Data.Monoid ((<>))
import       Hakyll
import Control.Applicative ((<|>))
import       Control.Monad                   (forM_)
import System.FilePath (replaceExtension, replaceDirectory, takeFileName, takeBaseName)
import System.Cmd (rawSystem, system)
import Data.List.Split (splitOneOf)
import Data.Strings (strStartsWith)
import           Data.Typeable (Typeable)
import           Data.Binary                   (Binary)

import Debug.Trace

--------------------------------------------------------------------------------

newtype Image = Image (Int,Int,FilePath)
  deriving (Show,Eq,Ord,Binary,Typeable)

instance Writable Image where
  write dst (Item _ (Image (w,h,src))) = let r = "convert -resize " ++ show w ++ "x" ++ show h ++ "\\> \"" ++ src ++ "\" \"" ++  dst ++ "\""
	in system r  >> return ()

imageResizeCompiler :: Int -> Int -> Compiler (Item Image)
imageResizeCompiler w h = getUnderlying >>= \y -> return (Item y $ Image (w,h,toFilePath y))

config :: Configuration
config = defaultConfiguration
  { deployCommand = "rsync -avr --delete _site/ torsnet6cs@sliceplorer.cs.univie.ac.at:evaluation_site/"
  }

datasets = [ "sinc2d", "ackley6d", "rosenbrock", "borehole"
           , "boston-svm", "boston-nn", "fuel", "neghip"]
techniques = ["ms", "ct", "ts", "hs", "sp"]

main :: IO ()
main = hakyllWith config $ do
  match "images/*.png" $ do
    route   idRoute
    compile $ imageResizeCompiler 350 350

  match "images/*.pdf" $ do
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

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  match "solutions/*.md" $ do
    route $ setExtension "html"
    compile $ do
      pandocCompiler
        -- >>= loadAndApplyTemplate "templates/task_solution.html" (exCtx exImgs)
        -- >>= loadAndApplyTemplate "templates/default.html" defaultContext
        -- >>= relativizeUrls

  match "tasks/*.md" $ do
    route $ setExtension "html"
    compile $ do
      --exImgs <- loadAll "images/*.png"
      --exDescs <- loadAll "solutions/*.html"
      pandocCompiler
        >>= loadAndApplyTemplate "templates/task_detail.html" taskCtx
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
taskCtx :: Context String
taskCtx =
  taskField "name" <>
  listField "datasets" datasetCtx (mapM makeItem datasets) <>
  defaultContext

datasetCtx :: Context String
datasetCtx = 
  bodyField "code" <>
  imgs <>
  --exs <>
  dsInfo
  where
  imgs = listFieldWith "imgs" imgTaskCtx $ \i -> do
    let tt = map (++ ("_" ++ itemBody i)) techniques :: [String]
    mapM makeItem tt
  {-exs = listFieldWith "explanations" exCtx $ \i -> do-}
    {-let tt = map (\t -> (traceShowId $ takeBaseName $ toFilePath $ itemIdentifier i) <> "_" <> t) techniques :: [String]-}
    {-mapM (\e -> load $ traceShowId $ fromFilePath ("/explanations/" ++ e ++ ".html")) tt-}
    --mapM makeItem tt
  dsInfo =
    Context $ \f a i ->
      let (Context c) = datasetInfo $ itemBody i
      in c f a i

exCtx = defaultContext

imgTaskCtx :: Context String
imgTaskCtx =
  bodyField "code"     <>
  imgTaskNameField "name" <>
  pngUrlField "pngUrl" <>
  pdfUrlField "pdfUrl"

imgTaskNameField :: String -> Context String
imgTaskNameField fld = field fld $ 
  return . humanizeTechniqueCode . getTechnique . itemBody
  where
  getTechnique = (!! 0) . splitOneOf "_."

pngUrlField :: String -> Context String
pngUrlField fld = field fld $ \i ->
  return $ "/images/" <> itemBody i <> ".png"

pdfUrlField :: String -> Context String
pdfUrlField fld = field fld $ \i ->
  return $ "/images/" <> itemBody i <> ".pdf"

imgField :: String -> Context String
imgField fld = field fld $ 
  return . itemImgPath

imgListField :: String -> Context CopyFile -> [Item CopyFile] -> Context a
imgListField fld ctx imgs = 
  listFieldWith fld ctx $ \i -> 
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
humanizeTaskCode "derive"       = "Compute derived value"
humanizeTaskCode "distribution" = "Characterize distribution"
humanizeTaskCode "extremum"     = "Find extrema"
humanizeTaskCode "filter"       = "Filter"
humanizeTaskCode "lookup"       = "Retrieve value"
humanizeTaskCode "range"        = "Determine range"
humanizeTaskCode code           = fail $ "Unknown task code: " <> code

humanizeTechniqueCode :: String -> String
humanizeTechniqueCode "ct" = "Contour tree"
humanizeTechniqueCode "hs" = "HyperSlice"
humanizeTechniqueCode "ms" = "Gerber et al."
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
  constField "dims" "5"
datasetInfo "borehole" =
  constField "name" "Borehole" <>
  constField "dims" "8"
datasetInfo "boston-svm" =
  constField "name" "SVM w/ radial basis" <>
  constField "dims" "13"
datasetInfo "boston-nn" =
  constField "name" "Neural network w/ 26 node hidden layer" <>
  constField "dims" "13"
datasetInfo "fuel" =
  constField "name" "Fuel dataset" <>
  constField "dims" "3"
datasetInfo "neghip" =
  constField "name" "Neghip dataset" <>
  constField "dims" "3"
datasetInfo code = mempty

-- combine 2 lists using the key functions
-- FIXME: not efficient at all!
innerJoin :: (Eq c) => (a -> c) -> (b -> c) -> [a] -> [b] -> [(a,b)]
innerJoin k1 k2 l1 l2 =
  concatMap pairKey l1
  where
  pairKey e1 = map (\e2 -> (e1, e2)) $ filter (\e2 -> k1 e1 == k2 e2) l2

