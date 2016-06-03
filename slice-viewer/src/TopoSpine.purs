module App.TopoSpine where

import Prelude
import Pux.Html (Html, div, h3, text, img)
import Pux.Html.Attributes (className, src)

view :: forall a. String -> Int -> Html a
view datasetName dims =
  div [className "topological-spine"]
    [ h3 [] [text "Topological spine"]
    , img [src imgSrc] []
    ]
  where
  imgSrc = "images/" ++ datasetName ++ "_" ++ (show dims) ++ ".png"

