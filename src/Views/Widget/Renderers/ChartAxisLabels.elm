module Views.Widget.Renderers.ChartAxisLabels exposing (renderXAxisLabel, renderYAxisLabel)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Views.Widget.Renderers.Config as ViewConfig exposing (ChartPadding)


createAxisLabel : Int -> Int -> Int -> Maybe String -> Svg msg
createAxisLabel x y rotation labelText =
    case labelText of
        Just labelText ->
            let
                transform =
                    "translate(" ++ (toString x) ++ "," ++ (toString y) ++ "), rotate(" ++ (toString rotation) ++ ")"
            in
                text_
                    [ Svg.Attributes.transform transform
                    , Svg.Attributes.textAnchor "middle"
                    , Svg.Attributes.fontSize "12"
                    , Svg.Attributes.fontWeight "bold"
                    ]
                    [ text labelText ]

        Nothing ->
            text ""


renderXAxisLabel : Int -> Int -> Maybe String -> ChartPadding -> Svg msg
renderXAxisLabel w h text chartPadding =
    let
        widthWithoutPadding =
            w - (round chartPadding.totalHorizontal)

        x =
            (widthWithoutPadding // 2) + (round chartPadding.left)

        y =
            h - 30
    in
        createAxisLabel x y 0 text


renderYAxisLabel : Int -> Maybe String -> ChartPadding -> Svg msg
renderYAxisLabel h text chartPadding =
    let
        heightWithoutPadding =
            h - (round chartPadding.totalVertical)

        x =
            20

        y =
            (heightWithoutPadding // 2) + (round chartPadding.top)
    in
        createAxisLabel x y -90 text
