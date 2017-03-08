 
{-# LANGUAGE OverloadedStrings #-}
import Clay
import Prelude hiding ((**))
import Data.Monoid ((<>))

site :: Css
site = do
  overviewPage
  taskPage
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
         do backgroundColor (rgb 242 240 247)
       star # ".color-code" # ".code-1" ?
         do backgroundColor (rgb 203 201 226)
       star # ".color-code" # ".code-2" ?
         do backgroundColor (rgb 158 154 200)
       star # ".color-code" # ".code-3" ?
         do backgroundColor (rgb 106  81 163)

exPage :: Css
exPage = do
  star # ".dataset-examples" ?
    do star # ".task-example" ?
         do h2 <> h3 ?
              do textAlign (alignSide sideCenter)
            ul ?
              do listStyleType none

taskPage :: Css
taskPage = do
  ul # ".examples" ?
    do "list-style" -: "none"
       display flex
       flexDirection row
       "flex-wrap" -: "none"
       "flex-shrink" -: "1"
       "flex-grow" -: "1"
       "justify-content" -: "space-between"
       "align-items" -: "flex-end"
       star # ".thumbnail" ** a ?
         do color black
            textDecoration none
            textAlign center
       star # ".thumbnail" ** h4 ?
         do fontSize (em 1.1)

main :: IO ()
main = putCss site

