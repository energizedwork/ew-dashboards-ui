module Views.Widget.Renderers.Metric exposing (render)

import Data.Widget as Widget exposing (Body, Widget)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.MetricAdapter as MetricAdapter
import Data.Widget.Config as RendererConfig
import Data.Widget.Table as Table exposing (Data)
import Html exposing (..)
import Html.Attributes exposing (class, title)
import Views.Widget.Renderers.Config as ViewConfig
import Views.Widget.Renderers.Utils as Utils


render : RendererConfig.Config -> Int -> Int -> Widget -> Table.Data -> Html msg
render optionalRendererConfig width height widget data =
    case widget.adapter of
        METRIC config ->
            let
                ( subtitle, actual, target, change, lastUpdated ) =
                    MetricAdapter.adapt config data

                calculatedWidth =
                    ViewConfig.calculateWidth optionalRendererConfig width

                calculatedHeight =
                    ViewConfig.calculateHeight optionalRendererConfig height
            in
                div
                    [ class <|
                        ViewConfig.colSpanClass optionalRendererConfig
                            ++ " widget container reduce-margin-top"
                    ]
                    [ div [ class "metric" ]
                        [ Utils.renderTitleFrom widget
                        , div [ class "metric-body" ]
                            [ p [ class "subtitle" ] [ text subtitle ]
                            , h3 [ class "actual" ] [ text actual ]
                            , h4 [ class "change" ] [ text change ]
                            , p [ class "forecast" ] [ text target ]
                            , p [ class "last-updated" ] [ text <| "updated: " ++ lastUpdated ]
                            ]
                        ]
                    ]

        _ ->
            p [ class "data" ] [ text "Sorry, I can only render metrics from a METRIC adapter right now" ]
