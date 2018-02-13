module Views.Widget.Renderers.ChartLegend
    exposing
        ( createHorizontalLabel
        , createVerticalLabel
        , createLabels
        , renderBottomCenterAligned
        , renderTopLeftAligned
        )

import Svg exposing (..)
import Svg.Attributes exposing (..)


labelOffset : Int
labelOffset =
    10


createLabel : Int -> String -> String -> (Int -> String) -> List (Attribute msg) -> Svg msg
createLabel index labelText labelPrefix getLabelColour attributes =
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


createHorizontalLabel : Int -> String -> String -> (Int -> String) -> Svg msg
createHorizontalLabel index labelText labelPrefix getLabelColor =
    createLabel index labelText labelPrefix getLabelColor [ Svg.Attributes.dx (toString labelOffset) ]


createVerticalLabel : Int -> String -> String -> (Int -> String) -> Svg msg
createVerticalLabel index labelText labelPrefix getLabelColor =
    createLabel index labelText labelPrefix getLabelColor [ Svg.Attributes.dy "20", Svg.Attributes.x (toString labelOffset) ]


createLabels : Maybe (List String) -> (Int -> String -> Svg msg) -> List (Svg msg)
createLabels seriesLabels renderLabel =
    case seriesLabels of
        Nothing ->
            []

        Just seriesLabels ->
            List.indexedMap renderLabel seriesLabels


createLegend :
    Int
    -> Int
    -> String
    -> List (Svg msg)
    -> List (Svg msg)
createLegend top left textAnchor labels =
    let
        xPosition =
            toString left

        yPosition =
            toString top

        transformAttribute =
            "translate(" ++ xPosition ++ ", " ++ yPosition ++ ")"
    in
        [ Svg.text_
            [ transform transformAttribute
            , Svg.Attributes.fontSize "12"
            , Svg.Attributes.fontWeight "bold"
            , Svg.Attributes.textAnchor textAnchor
            ]
            labels
        ]


renderBottomCenterAligned :
    Int
    -> Int
    -> List (Svg msg)
    -> List (Svg msg)
renderBottomCenterAligned width height labels =
    createLegend (height + 5) ((width // 2) - labelOffset) "middle" labels


renderTopLeftAligned :
    Int
    -> Int
    -> List (Svg msg)
    -> List (Svg msg)
renderTopLeftAligned top left labels =
    createLegend top left "left" labels
