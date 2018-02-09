module Views.Widget.Renderers.LineAndBarChart exposing (render)

import Data.Widget as Widget exposing (Body, Widget)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.LineAndBarAdapter as LineAndBarAdapter exposing (adapt)
import Data.Widget.Chart as Chart exposing (Data)
import Data.Widget.Config as RendererConfig
import Data.Widget.Table as Table exposing (..)
import Html exposing (..)
import Html.Attributes exposing (title)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Views.Widget.Renderers.BarChart as BarChart
import Views.Widget.Renderers.Config as ViewConfig
import Views.Widget.Renderers.LineChart as LineChart
import Views.Widget.Renderers.BarChart as BarChart
import Views.Widget.Renderers.Utils as Utils exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (..)
import Views.Widget.Renderers.ChartLegend as ChartLegend


padding : Float
padding =
    Utils.mediumPadding


render : RendererConfig.Config -> Int -> Int -> Widget -> Table.Data -> Html msg
render optionalRendererConfig width height widget data =
    case widget.adapter of
        LINE_AND_BAR_CHART optionalConfig ->
            let
                ( lineChart, barChart ) =
                    LineAndBarAdapter.adapt optionalConfig data

                calculatedWidth =
                    ViewConfig.calculateWidth optionalRendererConfig width

                calculatedHeight =
                    ViewConfig.calculateHeight optionalRendererConfig height
            in
                div [ class <| ViewConfig.colSpanClass optionalRendererConfig ++ " widget" ]
                    [ h3
                        [ Html.Attributes.title widget.description
                        , Html.Attributes.class "heading"
                        ]
                        [ Html.text widget.name ]
                    , view calculatedWidth calculatedHeight lineChart barChart
                    , Utils.renderDataSourceInfoFrom widget
                    ]

        _ ->
            p [ class "data" ]
                [ Html.text
                    "Sorry, I can only render line/bar combo charts from a LINE_AND_BAR_CHART adapter right now"
                ]


view : Int -> Int -> Chart.Data -> Chart.Data -> Html msg
view w h lineChart barChart =
    let
        firstLineDataTuple =
            List.head lineChart.data |> Maybe.withDefault []

        firstBarDataTuple =
            List.head barChart.data |> Maybe.withDefault []

        xTicksCount =
            List.length firstLineDataTuple

        yTicksCount =
            5

        defaultOptions =
            Axis.defaultOptions

        opts =
            { defaultOptions | orientation = Axis.Right, tickCount = yTicksCount }

        xAxisScale =
            (LineChart.xScale w firstLineDataTuple)

        yAxisScale =
            (LineChart.yScale h lineChart.maxValue)

        numBarRows =
            List.length barChart.rows

        renderedBarSeriesLabels =
            ChartLegend.renderLabels barChart.seriesLabels BarChart.renderLegendLabel

        renderedLineSeriesLabels =
            ChartLegend.renderLabels lineChart.seriesLabels LineChart.renderLegendLabel
    in
        svg
            [ Svg.Attributes.width (toString w ++ "px")
            , Svg.Attributes.height (toString h ++ "px")
            ]
            (List.concat
                [ [ LineChart.renderXAxis w h xTicksCount xAxisScale
                  , BarChart.renderYAxis w h barChart.maxValue
                  , LineChart.renderYAxis w h yAxisScale opts
                  , LineChart.renderYGrid w h lineChart.maxValue <|
                        Scale.ticks yAxisScale yTicksCount
                  ]
                , BarChart.renderColumns w
                    h
                    barChart.maxValue
                    numBarRows
                    barChart.indexedData
                , LineChart.renderLines w
                    h
                    lineChart.maxValue
                    firstLineDataTuple
                    lineChart.indexedData
                , ChartLegend.render (h - 10) (List.concat [ renderedLineSeriesLabels, renderedBarSeriesLabels ]) padding
                ]
            )
