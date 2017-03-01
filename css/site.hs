 
{-# LANGUAGE OverloadedStrings #-}
import Clay
import Data.Monoid ((<>))

site :: Css
site = do
  star # ".task-example" ?
    do h1 <> h2 ?
         do textAlign (alignSide sideCenter)
       ul ?
         do listStyleType none
            --border solid (px 1) grey

main :: IO ()
main = putCss site

