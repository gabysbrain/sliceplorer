--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import       Data.Monoid ((<>))
import       Hakyll
import       Control.Monad                   (forM_)
import System.FilePath (replaceExtension, replaceDirectory, takeFileName)
import Data.List.Split (splitOneOf)


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

  match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler

  match "css/*.hs" $ do
    route $ setExtension "css"
    compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

  -- create all the example pages
  {-forM_ taskExamples $ \ex ->-}
    {-create [exUrl ex] $ do-}
      {-route idRoute-}
      {-compile $ do-}
        {-let exContext = exCtx ex-}
        {-makeItem ""-}
          {->>= loadAndApplyTemplate "templates/task_example.html" exContext-}
          {->>= loadAndApplyTemplate "templates/default.html" exContext-}
          {->>= relativizeUrls-}

  match "examples/*.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/task_example.html" exCtx
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
exCtx :: Context String
exCtx =
  taskField      "task"      <>
  techniqueField "technique" <>
  imgField       "imgUrl"    <>
  defaultContext

imgField :: String -> Context String
imgField fld = field fld $ 
  return . itemImgPath

taskField :: String -> Context String
taskField fld = field fld $ 
  return . humanizeTaskCode . itemTaskCode

techniqueField :: String -> Context String
techniqueField fld = field fld $
  return . humanizeTechniqueCode . itemTechniqueCode

itemTaskCode =
  (!! 0) . (splitOneOf "_.") . takeFileName . toFilePath . itemIdentifier

itemTechniqueCode =
  (!! 1) . (splitOneOf "_.") . takeFileName . toFilePath . itemIdentifier

itemImgPath = 
  (`replaceDirectory` "/images/") . (`replaceExtension` "pdf") .
  toFilePath . itemIdentifier

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
humanizeTaskCode code           = error $ "Unknown task code: " <> code

humanizeTechniqueCode :: String -> String
humanizeTechniqueCode "ct" = "Contour tree"
humanizeTechniqueCode "hs" = "HyperSlice"
humanizeTechniqueCode "ms" = "Morse-smale complex (Gerber et al.)"
humanizeTechniqueCode "sp" = "1D slices"
humanizeTechniqueCode "ts" = "Topological spine"
humanizeTechniqueCode code = error $ "Unknown technique code: " <> code

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

