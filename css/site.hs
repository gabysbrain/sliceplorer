 
{-# LANGUAGE OverloadedStrings #-}
import Clay
import Prelude hiding ((**))
import Data.Monoid ((<>))

linkPopout = boxShadow 3 3 (px 4) (setA 30 black)

site :: Css
site = do
  overviewPage
  taskPage
  exPage

overviewPage :: Css
overviewPage = do
  thead ?
    do tr ?
         do height (em 8)
       th ?
         verticalAlign vAlignBottom
       th # ".task" ?
         do width (pct 20.0)
       th # ".description" ?
         do width (pct 40.0)
       th # ".technique" ?
         do transform $ rotate (deg 315)
            maxWidth (em 1)
            overflow visible
            --padding nil nil nil nil
  star # "#table-section" ?
    do table ?
         do fontSize (pct 60.0)
            "table-layout" -: "fixed"
       tbody |> tr # ":hover" ?
         do linkPopout
       td ?
         do height (pct 100.0)
       star # ".color-code" ?
         do height (em 1)
            width (em 1)
            display inlineBlock
            --lineHeight (px 0)
            --fontSize (px 0)
            color transparent
            border solid (px 1) (rgb 105 105 105)
            sym borderRadius (pct 50.0)
       td ** ".color-code" ?
         do height (em 2)
            width (em 2)
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
       --"flex-wrap" -: "none"
       flexGrow 1
       "justify-content" -: "space-between"
       "align-items" -: "flex-start"
       star # ".thumbnail" ?
         do flexGrow 1
       star # ".thumbnail" ?
         do a ?
             do color black
                textDecoration none
                textAlign center
            h4 ?
              do fontSize (em 1.1)
       --star # ".thumbnail" # ":hover" ?
         --do linkPopout

main :: IO ()
main = putCss site

