 
{-# LANGUAGE OverloadedStrings #-}
import Clay
import Prelude hiding ((**))
import Data.Monoid ((<>))

site :: Css
site = do
  overviewPage
  exPage

overviewPage :: Css
overviewPage = do
  th # ".task" ?
    do width (pct 20.0)
  th # ".description" ?
    do width (pct 40.0)
  th # ".technique" ?
    do transform $ rotate (deg 315)
       width (px 20)
       overflow visible
  star # "#table-section" ?
    do table ?
         do fontSize (pct 60.0)
            "table-layout" -: "fixed"
       td ?
         do height (pct 100.0)
       td ** a # ".color-code" ?
         do height (pct 100.0)
            display block
            lineHeight (px 0)
            fontSize (px 0)
            color transparent
       star # ".color-code" # ".code-0" ?
         do backgroundColor (rgb 254 235 226)
       star # ".color-code" # ".code-1" ?
         do backgroundColor (rgb 251 180 185)
       star # ".color-code" # ".code-2" ?
         do backgroundColor (rgb 247 104 161)
       star # ".color-code" # ".code-3" ?
         do backgroundColor (rgb 174   1 126)

exPage :: Css
exPage = do
  star # ".task-example" ?
    do h2 <> h3 ?
         do textAlign (alignSide sideCenter)
       ul ?
         do listStyleType none

main :: IO ()
main = putCss site

