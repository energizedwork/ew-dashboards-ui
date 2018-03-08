module Views.Widget.Renderers.LineAndBarChart exposing (render)

import Data.Widget as Widget exposing (Body, Widget)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.LineAndBarAdapter as LineAndBarAdapter exposing (adapt)
import Data.Widget.Adapters.TableAdapter exposing (Orientation(..))
import Data.Widget.Chart as Chart exposing (Data)
import Data.Widget.Config as RendererConfig
import Data.Widget.Table as Table exposing (..)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Views.Widget.Renderers.BarChart as BarChart
import Views.Widget.Renderers.Chart as ChartRenderer
import Views.Widget.Renderers.ChartAxisLabels as ChartAxisLabels
import Views.Widget.Renderers.ChartLegend as ChartLegend
import Views.Widget.Renderers.Config as ViewConfig exposing (ChartPadding, defaultChartPadding)
import Views.Widget.Renderers.LineChart as LineChart
import Views.Widget.Renderers.Utils as Utils exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (..)


chartPadding : ChartPadding
chartPadding =
    { defaultChartPadding
        | right = ViewConfig.largePadding
        , totalHorizontal = ViewConfig.largePadding * 2
    }


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

                namespace =
                    Utils.cssSafe widget.name
            in
                div [ class <| ViewConfig.colSpanClass optionalRendererConfig ++ " widget" ]
                    [ Utils.renderTitleFrom widget
                    , view namespace calculatedWidth calculatedHeight lineChart barChart
                    ]

        _ ->
            p [ class "data" ]
                [ Html.text
                    "Sorry, I can only render line/bar combo charts from a LINE_AND_BAR_CHART adapter right now"
                ]


view : String -> Int -> Int -> Chart.Data -> Chart.Data -> Html msg
view namespace w h lineChart barChart =
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
            (LineChart.xScale w firstLineDataTuple chartPadding)

        yAxisScale =
            (LineChart.yScale h lineChart.maxValue chartPadding)

        yGridTicks =
            Scale.ticks (LineChart.yScale h lineChart.maxValue chartPadding) yTicksCount

        numBarRows =
            List.length barChart.rows

        renderedBarSeriesLabels =
            ChartLegend.createLabels barChart.seriesLabels BarChart.legendLabel

        renderedLineSeriesLabels =
            ChartLegend.createLabels lineChart.seriesLabels LineChart.legendLabel

        forecastWidth =
            0

        actualsWidth =
            chartDimensions.w

        noPadding =
            Nothing

        chartDimensions =
            ChartRenderer.calculateDimensions w h (Just chartPadding)

        actualsDimensions =
            ChartRenderer.calculateDimensions actualsWidth chartDimensions.h noPadding

        forecastsDimensions =
            ChartRenderer.calculateDimensions forecastWidth chartDimensions.h noPadding
    in
        svg
            [ Svg.Attributes.width (toString w ++ "px")
            , Svg.Attributes.height (toString h ++ "px")
            ]
            (List.concat
                [ [ ChartRenderer.renderClipPaths namespace actualsDimensions forecastsDimensions
                  , LineChart.renderXAxis w h xTicksCount xAxisScale chartPadding
                  , BarChart.renderYAxis w h barChart.maxValue chartPadding
                  , LineChart.renderYAxis w h yAxisScale opts chartPadding
                  , Utils.renderYGrid w
                        h
                        chartPadding
                        lineChart.maxValue
                        (LineChart.yScale h lineChart.maxValue chartPadding)
                        yGridTicks
                  ]
                , BarChart.renderColumns w
                    h
                    barChart.maxValue
                    numBarRows
                    barChart.indexedData
                    chartPadding
                , LineChart.renderLines namespace
                    w
                    h
                    lineChart.maxValue
                    firstLineDataTuple
                    lineChart.indexedData
                    chartPadding
                    False
                , ChartLegend.renderBottomCenterAligned w
                    h
                    (List.concat
                        [ renderedLineSeriesLabels
                        , renderedBarSeriesLabels
                        ]
                    )
                , [ ChartAxisLabels.renderXAxisLabel w h lineChart.xAxisLabel chartPadding ]
                , [ ChartAxisLabels.renderLeftYAxisLabel h barChart.yAxisLabel chartPadding ]
                , [ ChartAxisLabels.renderRightYAxisLabel w h lineChart.yAxisLabel chartPadding ]
                ]
            )
