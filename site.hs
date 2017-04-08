--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
import       Data.Monoid ((<>))
import       Hakyll
import Control.Applicative (Alternative(empty), (<|>))
import       Control.Monad                   (forM_)
import System.FilePath (replaceExtension, replaceDirectory, takeFileName, takeBaseName)
import System.Cmd (rawSystem, system)
import Data.List.Split (splitOneOf)
import Data.Strings (strStartsWith)
import           Data.Typeable (Typeable)
import           Data.Binary                   (Binary)
import Data.Maybe (fromMaybe)

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
    compile pandocCompiler

  match "tasks/*.md" $ do
    route $ setExtension "html"
    compile $ do
      --tasks <- loadAll "tasks/*.md"
      let ctx = --listField "tasks" defaultContext (return tasks) <>
                taskCtx
      pandocCompiler
        >>= loadAndApplyTemplate "templates/task_detail.html" ctx
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
--taskCtx :: Context a
taskCtx = mconcat
  [ taskField "name"
  , field "prevTaskUrl" previousTaskUrl
  , field "nextTaskUrl" nextTaskUrl
  , field "prevTaskName" previousTaskName
  , field "nextTaskName" nextTaskName
  --, listField "tasks" defaultContext tasks
  , listField "techniques" techniqueCtx (mapM makeItem techniques)
  , listField "datasets" datasetCtx (mapM makeItem datasets)
  , listFieldWith "solutions" taskSolutionCtx taskSolutions
  , defaultContext
  ]

--datasetCtx :: Context a
datasetCtx = 
  bodyField "code" <>
  imgs <>
  dsInfo
  where
  imgs = listFieldWith "imgs" imgTaskCtx $ \i -> do
    let tt = map (++ ("_" ++ itemBody i)) techniques :: [String]
    mapM makeItem tt
  dsInfo =
    Context $ \f a i ->
      let (Context c) = datasetInfo $ itemBody i
      in c f a i

imgTaskCtx :: Context String
imgTaskCtx =
  bodyField "code"     <>
  imgTaskNameField "name" <>
  pngUrlField "pngUrl" <>
  pdfUrlField "pdfUrl"

techniqueCtx :: Context String
techniqueCtx = 
  bodyField "code" <>
  dsInfo
  where
  dsInfo =
    Context $ \f a i ->
      let (Context c) = constField "name" $ humanizeTechniqueCode $ itemBody i
      in c f a i

taskSolutionCtx = 
  techniqueCodeField "code" <>
  techniqueField "technique" <>
  defaultContext

taskSolutions task = mapM (taskSolution tc) techniques
  where
  tc = itemTaskCode task

taskSolution taskCd techniqueCd =
  load . fromFilePath $ "solutions/" <> taskCd <> "_" <> techniqueCd <> ".md"

taskName getTask task = do
  tasks <- getMatches "tasks/*.md"
  let myid = getTask tasks $ itemIdentifier task
  return $ maybe empty (humanizeTaskCode . taskCode) myid
  where
  taskCode = (!! 0) . splitOneOf "_." . takeFileName . toFilePath
  --return $ tn

taskUrl getTask task = do
  tasks <- getMatches "tasks/*.md"
  let myid = getTask tasks $ itemIdentifier task
  maybe empty (fmap (maybe empty toUrl) . getRoute) myid

previousTaskName :: Item String -> Compiler String
previousTaskName = taskName itemBefore

nextTaskName :: Item String -> Compiler String
nextTaskName = taskName itemAfter

previousTaskUrl :: Item String -> Compiler String
previousTaskUrl = taskUrl itemBefore

nextTaskUrl :: Item String -> Compiler String
nextTaskUrl = taskUrl itemAfter

itemAfter xs x = lookup x $ zip xs (tail xs <> [head xs])
itemBefore xs x = lookup x $ zip (tail xs <> [head xs]) xs

imgTaskNameField :: String -> Context String
imgTaskNameField fld = field fld $ 
  return . humanizeTechniqueCode . getTechnique . itemBody
  where
  getTechnique = (!! 0) . splitOneOf "_."

pngUrlField :: String -> Context String
pngUrlField fld = field fld $ \i -> do
  imgRoute <- getRoute $ fromFilePath $ "images/" <> itemBody i <> ".png"
  noImgRoute <- getRoute $ fromFilePath "images/not_found.png"
  case noImgRoute of
    Just rt -> return $ toUrl (fromMaybe rt imgRoute)
    Nothing -> fail "can't find no example image (images/not_found.png)"

pdfUrlField :: String -> Context String
pdfUrlField fld = field fld $ \i -> do
  imgRoute <- getRoute $ fromFilePath $ "images/" <> itemBody i <> ".pdf"
  return $ maybe empty toUrl imgRoute

--taskField :: String -> Context a
taskField fld = field fld $ 
  return . humanizeTaskCode . itemTaskCode

techniqueField fld = field fld $
  return . humanizeTechniqueCode . itemTechniqueCode

techniqueCodeField fld = field fld $
  return . itemTechniqueCode

itemTaskCode = itemCode 0

itemTechniqueCode = itemCode 1

itemTaskTechniqueCode i =
  itemTaskCode i <> "_" <> itemTechniqueCode i

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

--datasetInfo :: String -> Context a
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

