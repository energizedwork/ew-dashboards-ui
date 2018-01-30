module Views.Widget.Renderers.LineAndBarChart exposing (render)

import Data.Widget as Widget exposing (Body, Widget)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.LineAndBarAdapter as LineAndBarAdapter exposing (adapt)
import Data.Widget.Chart as Chart exposing (Data)
import Data.Widget.Table as Table exposing (..)
import Html exposing (..)
import Html.Attributes exposing (title)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Views.Widget.Renderers.BarChart as BarChart
import Views.Widget.Renderers.LineChart as LineChart
import Views.Widget.Renderers.Utils as Utils exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)


render : Int -> Int -> Widget -> Table.Data -> Html msg
render width height widget data =
    case widget.adapter of
        LINE_AND_BAR_CHART optionalConfig ->
            let
                ( lineChart, barChart ) =
                    LineAndBarAdapter.adapt optionalConfig data
            in
                div [ class "col-md-6 widget" ]
                    [ h3
                        [ Html.Attributes.title widget.description
                        , Html.Attributes.class "heading"
                        ]
                        [ Html.text widget.name ]
                    , view (width // 2) (height // 2) lineChart barChart
                    , Utils.renderDataSourceInfoFrom widget
                    ]

        _ ->
            p [ class "data" ]
                [ Html.text
                    "Sorry, I can only render line/bar combo charts from a LINE_AND_BAR_CHART adapter right now"
                ]


view : Int -> Int -> Chart.Data -> Chart.Data -> Html msg
view width height lineChart barChart =
    let
        firstLineDataTuple =
            List.head lineChart.data |> Maybe.withDefault []

        firstBarDataTuple =
            List.head barChart.data |> Maybe.withDefault []

        numTicks =
            List.length firstLineDataTuple

        defaultOptions =
            Axis.defaultOptions

        opts =
            { defaultOptions | orientation = Axis.Right, tickCount = 10 }

        xAxisScale =
            (LineChart.xScale width firstLineDataTuple)

        yAxisScale =
            (LineChart.yScale height lineChart.maxValue)

        numBarRows =
            List.length barChart.rows
    in
        svg
            [ Svg.Attributes.width (toString width ++ "px")
            , Svg.Attributes.height (toString height ++ "px")
            ]
            (List.concat
                [ [ LineChart.renderXAxis width height numTicks xAxisScale
                  , BarChart.renderYAxis width height barChart.maxValue
                  , LineChart.renderYAxis width height yAxisScale opts
                  ]
                , BarChart.renderColumns width height barChart.maxValue numBarRows barChart.indexedData
                , LineChart.renderLines width height lineChart.maxValue firstLineDataTuple lineChart.indexedData
                ]
            )
