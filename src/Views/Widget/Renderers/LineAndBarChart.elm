module Views.Widget.Renderers.LineAndBarChart exposing (render)

import Data.Widget as Widget exposing (Body, Widget)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.LineAndBarAdapter as LineAndBarAdapter exposing (adapt)
import Data.Widget.Chart as Chart exposing (Data)
import Data.Widget.Config as RendererConfig
import Data.Widget.Table as Table exposing (..)
import Html exposing (..)
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
import Views.Widget.Renderers.ChartAxisLabels as ChartAxisLabels


padding : Float
padding =
    ViewConfig.largePadding


render : RendererConfig.Config -> Int -> Int -> Widget -> Table.Data -> Html msg
render optionalRendererConfig width height widget data =
    case widget.adapter of
        LINE_AND_BAR_CHART optionalConfig ->
            let
                ( lineChart, barChart, xAxisLabel, yAxisLabel ) =
                    LineAndBarAdapter.adapt optionalConfig data

                calculatedWidth =
                    ViewConfig.calculateWidth optionalRendererConfig width

                calculatedHeight =
                    ViewConfig.calculateHeight optionalRendererConfig height
            in
                div [ class <| ViewConfig.colSpanClass optionalRendererConfig ++ " widget" ]
                    [ Utils.renderTitleFrom widget
                    , view calculatedWidth calculatedHeight lineChart barChart xAxisLabel yAxisLabel
                    ]

        _ ->
            p [ class "data" ]
                [ Html.text
                    "Sorry, I can only render line/bar combo charts from a LINE_AND_BAR_CHART adapter right now"
                ]


view : Int -> Int -> Chart.Data -> Chart.Data -> Maybe String -> Maybe String -> Html msg
view w h lineChart barChart xAxisLabel yAxisLabel =
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

        yGridTicks =
            Scale.ticks (LineChart.yScale h lineChart.maxValue) yTicksCount

        numBarRows =
            List.length barChart.rows

        renderedBarSeriesLabels =
            ChartLegend.createLabels barChart.seriesLabels BarChart.legendLabel

        renderedLineSeriesLabels =
            ChartLegend.createLabels lineChart.seriesLabels LineChart.legendLabel
    in
        svg
            [ Svg.Attributes.width (toString w ++ "px")
            , Svg.Attributes.height (toString h ++ "px")
            ]
            (List.concat
                [ [ LineChart.renderXAxis w h xTicksCount xAxisScale
                  , BarChart.renderYAxis w h barChart.maxValue
                  , LineChart.renderYAxis w h yAxisScale opts
                  , Utils.renderYGrid w
                        h
                        padding
                        lineChart.maxValue
                        (LineChart.yScale h lineChart.maxValue)
                        yGridTicks
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
                , ChartLegend.renderBottomCenterAligned w
                    h
                    (List.concat
                        [ renderedLineSeriesLabels
                        , renderedBarSeriesLabels
                        ]
                    )
                , [ ChartAxisLabels.renderXAxisLabel w h xAxisLabel ]
                , [ ChartAxisLabels.renderYAxisLabel h yAxisLabel ]
                ]
            )
