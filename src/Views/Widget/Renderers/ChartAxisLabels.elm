module Views.Widget.Renderers.ChartAxisLabels exposing (renderXAxisLabel, renderYAxisLabel)

import Svg exposing (..)
import Svg.Attributes exposing (..)


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


renderXAxisLabel : Int -> Int -> Maybe String -> Svg msg
renderXAxisLabel width height text =
    createAxisLabel (width // 2) (height - 30) 0 text


renderYAxisLabel : Int -> Maybe String -> Svg msg
renderYAxisLabel height text =
    createAxisLabel 20 (height // 2) -90 text
