module Views.Widget.Renderers.Config
    exposing
        ( calculateWidth
        , mediumWidth
        , mediumHeight
        , mediumPadding
        , largeWidth
        , largeHeight
        , largePadding
        , calculateHeight
        , colSpan
        , colSpanClass
        , defaultColSpan
        , totalCols
        , defaultChartPadding
        , ChartPadding
        )

import Data.Widget.Config as WidgetConfig
import Dict
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)


type alias ChartPadding =
    { bottom : Float
    , left : Float
    , right : Float
    , top : Float
    , totalVertical : Float
    , totalHorizontal : Float
    }


mediumWidth : Float
mediumWidth =
    740


mediumHeight : Float
mediumHeight =
    370


mediumPadding : Float
mediumPadding =
    50


largeWidth : Float
largeWidth =
    1320


largeHeight : Float
largeHeight =
    880


largePadding : Float
largePadding =
    70


defaultColSpan : Int
defaultColSpan =
    totalCols


defaultChartPadding : ChartPadding
defaultChartPadding =
    { top = mediumPadding
    , right = mediumPadding
    , bottom = largePadding
    , left = largePadding
    , totalVertical = mediumPadding + largePadding
    , totalHorizontal = mediumPadding + largePadding
    }


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
