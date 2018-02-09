module Views.Widget.Renderers.ChartLegend exposing (renderHorizontalLabel, renderVerticalLabel, renderLabels, render)

import Svg exposing (..)
import Svg.Attributes exposing (..)


renderLabel : Int -> String -> String -> (Int -> String) -> List (Attribute msg) -> Svg msg
renderLabel index labelText labelPrefix getLabelColour attributes =
    let
        labelColour =
            getLabelColour index

        key =
            tspan [ Svg.Attributes.fontSize "38", Svg.Attributes.baselineShift "middle" ] [ Svg.text labelPrefix ]

        label =
            tspan [ Svg.Attributes.baselineShift "super" ] [ Svg.text labelText ]

        tspanAttributes =
            List.concat [ attributes, [ fill labelColour, Svg.Attributes.baselineShift "super" ] ]
    in
        tspan tspanAttributes [ key, label ]


renderHorizontalLabel : Int -> String -> String -> (Int -> String) -> Svg msg
renderHorizontalLabel index labelText labelPrefix getLabelColor =
    renderLabel index labelText labelPrefix getLabelColor [ Svg.Attributes.dx "10" ]


renderVerticalLabel : Int -> String -> String -> (Int -> String) -> Svg msg
renderVerticalLabel index labelText labelPrefix getLabelColor =
    renderLabel index labelText labelPrefix getLabelColor [ Svg.Attributes.dy "20", Svg.Attributes.x "10" ]


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
