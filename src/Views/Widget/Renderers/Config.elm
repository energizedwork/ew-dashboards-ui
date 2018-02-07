module Views.Widget.Renderers.Config exposing (calculateWidth, calculateHeight, colSpan, colSpanClass, defaultColSpan, totalCols)

import Data.Widget.Config as WidgetConfig
import Dict
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)


defaultColSpan : Int
defaultColSpan =
    totalCols


totalCols : Int
totalCols =
    12


colSpan : WidgetConfig.Config -> Int
colSpan config =
    (Dict.get "colSpan" config)
        |> Maybe.withDefault (Encode.int defaultColSpan)
        |> Decode.decodeValue Decode.int
        |> Result.withDefault defaultColSpan


colSpanClass : WidgetConfig.Config -> String
colSpanClass config =
    colSpan config
        |> toString
        |> (++) "col-md-"


calculateWidth : WidgetConfig.Config -> Int -> Int
calculateWidth config width =
    width
        // (totalCols // colSpan config)


calculateHeight : WidgetConfig.Config -> Int -> Int
calculateHeight config height =
    height
        // 3
