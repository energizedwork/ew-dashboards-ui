module Views.Spinner exposing (spinner, spinnerSVG, wrappedSpinnerSVG)

import Html exposing (Attribute, Html, div, li, text)
import Html.Attributes as HTMLa exposing (class, style, attribute)
import Svg exposing (svg, node)
import Svg.Attributes as SVGa exposing (..)
import Util exposing ((=>))


spinner : Html msg
spinner =
    li [ HTMLa.class "sk-three-bounce", HTMLa.style [ "float" => "left", "margin" => "8px" ] ]
        [ div [ HTMLa.class "sk-child sk-bounce1" ] []
        , div [ HTMLa.class "sk-child sk-bounce2" ] []
        , div [ HTMLa.class "sk-child sk-bounce3" ] []
        ]


wrappedSpinnerSVG : Html msg
wrappedSpinnerSVG =
    div [ HTMLa.class "spinner-wrapper" ] [ spinnerSVG ]


spinnerSVG : Html msg
spinnerSVG =
    svg [ SVGa.class "spinner", attribute "height" "65px", attribute "preserveAspectRatio" "xMidYMid", viewBox "0 0 100 100", attribute "width" "65px", attribute "xmlns" "http://www.w3.org/2000/svg" ]
        [ node "rect"
            [ SVGa.class "bk", fill "none", attribute "height" "100", attribute "width" "100", attribute "x" "0", attribute "y" "0" ]
            []
        , node "rect"
            [ fill "black", attribute "height" "20", attribute "rx" "5", attribute "ry" "5", attribute "transform" "rotate(0 50 50) translate(0 -30)", attribute "width" "7", attribute "x" "46.5", attribute "y" "40" ]
            [ node "animate"
                [ attribute "attributeName" "opacity", attribute "begin" "0s", attribute "dur" "1s", attribute "from" "1", attribute "repeatCount" "indefinite", attribute "to" "0" ]
                []
            , text "      "
            ]
        , node "rect"
            [ fill "black", attribute "height" "20", attribute "rx" "5", attribute "ry" "5", attribute "transform" "rotate(30 50 50) translate(0 -30)", attribute "width" "7", attribute "x" "46.5", attribute "y" "40" ]
            [ node "animate"
                [ attribute "attributeName" "opacity", attribute "begin" "0.08333333333333333s", attribute "dur" "1s", attribute "from" "1", attribute "repeatCount" "indefinite", attribute "to" "0" ]
                []
            , text "      "
            ]
        , node "rect"
            [ fill "black", attribute "height" "20", attribute "rx" "5", attribute "ry" "5", attribute "transform" "rotate(60 50 50) translate(0 -30)", attribute "width" "7", attribute "x" "46.5", attribute "y" "40" ]
            [ node "animate"
                [ attribute "attributeName" "opacity", attribute "begin" "0.16666666666666666s", attribute "dur" "1s", attribute "from" "1", attribute "repeatCount" "indefinite", attribute "to" "0" ]
                []
            , text "        "
            ]
        , node "rect"
            [ fill "black", attribute "height" "20", attribute "rx" "5", attribute "ry" "5", attribute "transform" "rotate(90 50 50) translate(0 -30)", attribute "width" "7", attribute "x" "46.5", attribute "y" "40" ]
            [ node "animate"
                [ attribute "attributeName" "opacity", attribute "begin" "0.25s", attribute "dur" "1s", attribute "from" "1", attribute "repeatCount" "indefinite", attribute "to" "0" ]
                []
            , text "          "
            ]
        , node "rect"
            [ fill "black", attribute "height" "20", attribute "rx" "5", attribute "ry" "5", attribute "transform" "rotate(120 50 50) translate(0 -30)", attribute "width" "7", attribute "x" "46.5", attribute "y" "40" ]
            [ node "animate"
                [ attribute "attributeName" "opacity", attribute "begin" "0.3333333333333333s", attribute "dur" "1s", attribute "from" "1", attribute "repeatCount" "indefinite", attribute "to" "0" ]
                []
            , text "          "
            ]
        , node "rect"
            [ fill "black", attribute "height" "20", attribute "rx" "5", attribute "ry" "5", attribute "transform" "rotate(150 50 50) translate(0 -30)", attribute "width" "7", attribute "x" "46.5", attribute "y" "40" ]
            [ node "animate"
                [ attribute "attributeName" "opacity", attribute "begin" "0.4166666666666667s", attribute "dur" "1s", attribute "from" "1", attribute "repeatCount" "indefinite", attribute "to" "0" ]
                []
            , text "          "
            ]
        , node "rect"
            [ fill "black", attribute "height" "20", attribute "rx" "5", attribute "ry" "5", attribute "transform" "rotate(180 50 50) translate(0 -30)", attribute "width" "7", attribute "x" "46.5", attribute "y" "40" ]
            [ node "animate"
                [ attribute "attributeName" "opacity", attribute "begin" "0.5s", attribute "dur" "1s", attribute "from" "1", attribute "repeatCount" "indefinite", attribute "to" "0" ]
                []
            , text "          "
            ]
        , node "rect"
            [ fill "black", attribute "height" "20", attribute "rx" "5", attribute "ry" "5", attribute "transform" "rotate(210 50 50) translate(0 -30)", attribute "width" "7", attribute "x" "46.5", attribute "y" "40" ]
            [ node "animate"
                [ attribute "attributeName" "opacity", attribute "begin" "0.5833333333333334s", attribute "dur" "1s", attribute "from" "1", attribute "repeatCount" "indefinite", attribute "to" "0" ]
                []
            , text "          "
            ]
        , node "rect"
            [ fill "black", attribute "height" "20", attribute "rx" "5", attribute "ry" "5", attribute "transform" "rotate(240 50 50) translate(0 -30)", attribute "width" "7", attribute "x" "46.5", attribute "y" "40" ]
            [ node "animate"
                [ attribute "attributeName" "opacity", attribute "begin" "0.6666666666666666s", attribute "dur" "1s", attribute "from" "1", attribute "repeatCount" "indefinite", attribute "to" "0" ]
                []
            , text "          "
            ]
        , node "rect"
            [ fill "black", attribute "height" "20", attribute "rx" "5", attribute "ry" "5", attribute "transform" "rotate(270 50 50) translate(0 -30)", attribute "width" "7", attribute "x" "46.5", attribute "y" "40" ]
            [ node "animate"
                [ attribute "attributeName" "opacity", attribute "begin" "0.75s", attribute "dur" "1s", attribute "from" "1", attribute "repeatCount" "indefinite", attribute "to" "0" ]
                []
            , text "          "
            ]
        , node "rect"
            [ fill "black", attribute "height" "20", attribute "rx" "5", attribute "ry" "5", attribute "transform" "rotate(300 50 50) translate(0 -30)", attribute "width" "7", attribute "x" "46.5", attribute "y" "40" ]
            [ node "animate"
                [ attribute "attributeName" "opacity", attribute "begin" "0.8333333333333334s", attribute "dur" "1s", attribute "from" "1", attribute "repeatCount" "indefinite", attribute "to" "0" ]
                []
            , text "          "
            ]
        , node "rect"
            [ fill "black", attribute "height" "20", attribute "rx" "5", attribute "ry" "5", attribute "transform" "rotate(330 50 50) translate(0 -30)", attribute "width" "7", attribute "x" "46.5", attribute "y" "40" ]
            [ node "animate"
                [ attribute "attributeName" "opacity", attribute "begin" "0.9166666666666666s", attribute "dur" "1s", attribute "from" "1", attribute "repeatCount" "indefinite", attribute "to" "0" ]
                []
            , text "          "
            ]
        ]
