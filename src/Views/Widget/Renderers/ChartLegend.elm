module Views.Widget.Renderers.ChartLegend exposing (renderLabel, render)

import Svg exposing (..)
import Svg.Attributes exposing (..)


renderLabel : Int -> String -> String -> (Int -> String) -> Svg msg
renderLabel index labelText labelPrefix getLabelColour =
    let
        labelColour =
            getLabelColour index

        label =
            labelPrefix ++ " " ++ labelText
    in
        tspan [ Svg.Attributes.dx "10", fill labelColour ] [ Svg.text label ]


render :
    Int
    -> Maybe (List String)
    -> (Int -> String -> Svg msg)
    -> Float
    -> List (Svg msg)
render top seriesLabels renderLabels padding =
    case seriesLabels of
        Nothing ->
            []

        Just seriesLabels ->
            let
                labels =
                    List.indexedMap renderLabels seriesLabels

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
