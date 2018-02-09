module Views.Widget.Renderers.ChartLegend exposing (renderLabel, renderLabels, render)

import Svg exposing (..)
import Svg.Attributes exposing (..)


renderLabel : Int -> String -> String -> (Int -> String) -> Svg msg
renderLabel index labelText labelPrefix getLabelColour =
    let
        labelColour =
            getLabelColour index

        key =
            tspan [ Svg.Attributes.fontSize "38", Svg.Attributes.baselineShift "middle" ] [ Svg.text labelPrefix ]

        label =
            tspan [ Svg.Attributes.baselineShift "super" ] [ Svg.text labelText ]
    in
        tspan [ Svg.Attributes.dx "10", fill labelColour, Svg.Attributes.baselineShift "super" ] [ key, label ]


renderLabels : Maybe (List String) -> (Int -> String -> Svg msg) -> List (Svg msg)
renderLabels seriesLabels renderLabel =
    case seriesLabels of
        Nothing ->
            []

        Just seriesLabels ->
            List.indexedMap renderLabel seriesLabels


render :
    Int
    -> List (Svg msg)
    -> Float
    -> List (Svg msg)
render top labels padding =
    let
        xPosition =
            toString padding

        yPosition =
            toString top

        transformAttribute =
            "translate(" ++ xPosition ++ ", " ++ yPosition ++ ")"
    in
        [ Svg.text_
            [ transform transformAttribute
            , Svg.Attributes.fontSize "12"
            , Svg.Attributes.fontWeight "bold"
            ]
            labels
        ]
