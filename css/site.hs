 
{-# LANGUAGE OverloadedStrings #-}
import Clay
import Prelude hiding ((**), span)
import Data.Monoid ((<>))

linkPopout = boxShadow (px 3) (px 3) (px 9) (setA 0.5 black)

site :: Css
site = do
  overviewPage
  taskPage
  exPage

overviewPage :: Css
overviewPage = do
  thead ?
    do th ?
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
         do "table-layout" -: "fixed"
            marginTop (em 4)
       tbody |> tr # ":hover" ?
         do linkPopout
       td ?
         do height (pct 100.0)
       star # ".color-code" ?
         do height (em 1.5)
            width (em 1.5)
            display inlineBlock
            --lineHeight (px 0)
            --fontSize (px 0)
            color transparent
            border solid (px 1) (rgb 105 105 105)
            sym borderRadius (pct 50.0)
       td <> th ?
         do padding (em 0.4) (em 0.6) (em 0.4) (em 0.6)
       star # ".color-code" # ".code-0" ?
         do backgroundColor (rgb 242 240 247)
       star # ".color-code" # ".code-1" ?
         do backgroundColor (rgb 203 201 226)
       star # ".color-code" # ".code-2" ?
         do backgroundColor (rgb 158 154 200)
       star # ".color-code" # ".code-3" ?
         do backgroundColor (rgb 106  81 163)
       star # ".legend" ?
         do listStyleType none
            span # ".color-code" ?
              do marginRight (em 0.5)
                 verticalAlign middle

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
  star # ".controls" ** label ? do
    display inline
  star # ".technique-examples" ? do 
    star # ".row" ? do 
      display flex
      flexDirection row
      flexGrow 1
      "justify-content" -: "space-between"
      "align-items" -: "flex-start"
    h4 ? do 
      fontSize (em 1.5)
      textAlign center
      a ?
        do color inherit
           textDecoration inherit
    
  ul # ".examples" ?
    do "list-style" -: "none"
       display flex
       flexDirection row
       flexGrow 1
       "justify-content" -: "space-between"
       "align-items" -: "flex-start"
       star # ".thumbnail" ?
         do flexGrow 1
            a ?
              do color inherit
                 textDecoration inherit
       star # ".thumbnail" # ":hover" ?
         do linkPopout
            zIndex 10 -- to the top!


main :: IO ()
main = putCss site

