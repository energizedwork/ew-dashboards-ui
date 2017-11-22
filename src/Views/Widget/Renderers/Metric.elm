module Views.Widget.Renderers.Metric exposing (render)

import Data.Widget as Widget exposing (Widget, Body)
import Data.Widget.Table as Table exposing (Data)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.MetricAdapter as MetricAdapter
import Html exposing (..)
import Html.Attributes exposing (class, title)


viewMetric : String -> String -> Html msg
viewMetric label metric =
    div [ class "card d-inline-block" ]
        [ div [ class "card-header" ]
            [ text label
            ]
        , div [ class "card-block" ]
            [ div [ class "" ]
                [ div [ class "h1 font-weight-bold" ] [ text metric ]
                ]
            ]
        ]


render : Int -> Int -> Widget Body -> Table.Data -> Html msg
render width height widget data =
    case widget.adapter of
        METRIC config ->
            let
                ( source, target ) =
                    MetricAdapter.adapt config data
            in
                div
                    [ class "col-md-12 widget container" ]
                    [ h3 [ title widget.description, class "heading" ] [ Html.text widget.name ]
                    , viewMetric "Actual" source
                    , viewMetric "Target" target
                    ]

        _ ->
            p [ class "data" ] [ text "Sorry, I can only render metrics from a METRIC adapter right now" ]
