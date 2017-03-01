 
{-# LANGUAGE OverloadedStrings #-}
import Clay
import Data.Monoid ((<>))

site :: Css
site = do
  overviewPage
  exPage

overviewPage :: Css
overviewPage = do
  th # ".technique" ?
    do transform $ rotate (deg 315)
  star # "#table-section" ?
    do table ?
         do fontSize (pct 60.0)

exPage :: Css
exPage = do
  star # ".task-example" ?
    do h2 <> h3 ?
         do textAlign (alignSide sideCenter)
       ul ?
         do listStyleType none
            --border solid (px 1) grey

main :: IO ()
main = putCss site

