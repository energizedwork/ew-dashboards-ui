module Views.Widget.Renderers.Metric exposing (render)

import Data.Widget as Widget exposing (Widget, Body)
import Data.Widget.Table as Table exposing (Data, Cell)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.MetricAdapter as MetricAdapter
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, src, title, style)


render : Int -> Int -> Widget Body -> Table.Data -> Html msg
render width height widget data =
    case widget.adapter of
        METRIC ->
            let
                ( source, target ) =
                    MetricAdapter.adapt MetricAdapter.defaultConfig data
            in
                div
                    [ class <| "col-md-12 widget" ]
                    [ h3 [ title widget.description, class "heading" ] [ Html.text widget.name ]
                    , span [] [ text source ]
                    , span [] [ text " / " ]
                    , span [] [ text target ]
                    ]

        _ ->
            p [ class "data" ] [ text "Sorry, I can only render tables from a METRIC adapter right now" ]
