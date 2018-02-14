module Views.Widget.Renderers.LineChart
    exposing
        ( render
        , renderLines
        , renderLine
        , renderXAxis
        , renderYAxis
        , xScale
        , yScale
        , renderLegend
        , legendLabel
        )

import Array exposing (..)
import Color
import Color.Convert
import Data.Widget as Widget exposing (Body, Widget)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Config as RendererConfig
import Data.Widget.Adapters.ChartAdapter as ChartAdapter
import Data.Widget.Table as Table exposing (Cell, Data)
import Html exposing (..)
import NumberParser
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Views.Widget.Renderers.Config as ViewConfig exposing (defaultChartPadding)
import Views.Widget.Renderers.Utils as Utils exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Visualization.Shape as Shape
import Views.Widget.Renderers.ChartLegend as ChartLegend
import Views.Widget.Renderers.ChartAxisLabels as ChartAxisLabels
import Data.Widget.Chart as Chart


render : RendererConfig.Config -> Int -> Int -> Widget -> Table.Data -> Html msg
render optionalRendererConfig width height widget data =
    case widget.adapter of
        CHART optionalAdapterConfig ->
            let
                chartData =
                    ChartAdapter.adapt optionalAdapterConfig data

                calculatedWidth =
                    ViewConfig.calculateWidth optionalRendererConfig width

                calculatedHeight =
                    ViewConfig.calculateHeight optionalRendererConfig height
            in
                div
                    [ class <|
                        ViewConfig.colSpanClass optionalRendererConfig
                            ++ " widget"
                    ]
                    [ Utils.renderTitleFrom widget
                    , view calculatedWidth calculatedHeight chartData
                    ]

        _ ->
            p [ class "data" ] [ Html.text "Sorry, I can only render line charts from a CHART adapter right now" ]


lineColours : Array Color.Color
lineColours =
    Array.fromList (List.reverse Scale.category10)


getLineColour : Int -> String
getLineColour index =
    get index lineColours
        |> Maybe.withDefault Color.black
        |> Color.Convert.colorToHex


view : Int -> Int -> Chart.Data -> Svg msg
view w h chartData =
    let
        firstRow =
            List.head chartData.data
                |> Maybe.withDefault []

        indexedData =
            Array.toIndexedList (Array.fromList chartData.data)

        xTicksCount =
            List.length firstRow

        yTicksCount =
            5

        defaultOptions =
            Axis.defaultOptions

        opts =
            { defaultOptions
                | orientation = Axis.Left
                , tickCount = yTicksCount
                , tickSizeOuter = 0
            }

        yGridTicks =
            Scale.ticks (yScale h chartData.maxValue) yTicksCount
    in
        svg [ Svg.Attributes.width (toString w ++ "px"), Svg.Attributes.height (toString h ++ "px") ]
            (List.concat
                [ [ renderXAxis w h xTicksCount (xScale w firstRow)
                  , renderYAxis w h (yScale h chartData.maxValue) opts
                  , Utils.renderYGrid w h defaultChartPadding chartData.maxValue (yScale h chartData.maxValue) yGridTicks
                  ]
                , renderLines w h chartData.maxValue firstRow indexedData
                , renderLegend w h chartData.seriesLabels
                , [ ChartAxisLabels.renderXAxisLabel w h chartData.xAxisLabel defaultChartPadding ]
                , [ ChartAxisLabels.renderYAxisLabel h chartData.yAxisLabel defaultChartPadding ]
                ]
            )


renderLines :
    Int
    -> Int
    -> Float
    -> List ( Cell, Cell )
    -> List ( Int, List ( Cell, Cell ) )
    -> List (Svg msg)
renderLines width height maxValue firstRow indexedData =
    List.map
        (\( index, rowTuple ) ->
            generateSVGPathDesc width height maxValue firstRow rowTuple
                |> renderLine (getLineColour (index))
        )
        indexedData


renderLine : String -> String -> Svg msg
renderLine colour lineData =
    g [ transform ("translate(78" ++ ", " ++ toString defaultChartPadding.top ++ ")"), class "series" ]
        [ Svg.path [ d lineData, stroke colour, strokeWidth "3px", fill "none" ] []
        ]


renderXAxis : Int -> Int -> Int -> BandScale a1 -> Svg msg
renderXAxis width height numTicks bandScale =
    let
        opts =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Axis.axis
                { opts
                    | orientation = Axis.Bottom
                    , tickFormat = Just Utils.formatStringTick
                    , tickCount = numTicks
                    , tickSizeOuter = 0
                }
                (Scale.toRenderable <| bandScale)
    in
        g [ transform ("translate(" ++ toString (defaultChartPadding.left - 1) ++ ", " ++ toString (toFloat height - defaultChartPadding.bottom) ++ ")") ]
            [ xAxis ]


renderYAxis :
    Int
    -> b
    -> Axis.RenderableScale a domain range value
    -> Axis.Options value
    -> Svg msg
renderYAxis width height continuousScale opts =
    let
        yAxis : Svg msg
        yAxis =
            Axis.axis opts continuousScale

        xTranslate =
            case opts.orientation of
                Axis.Left ->
                    floor <| defaultChartPadding.left - 1

                Axis.Right ->
                    width - (floor (defaultChartPadding.right) + 1)

                Axis.Top ->
                    floor <| defaultChartPadding.top - 1

                Axis.Bottom ->
                    floor <| defaultChartPadding.top - 1
    in
        g
            [ transform
                ("translate("
                    ++ toString (xTranslate)
                    ++ ", "
                    ++ toString defaultChartPadding.top
                    ++ ")"
                )
            ]
            [ yAxis ]


legendLabel : Int -> String -> Svg msg
legendLabel index labelText =
    ChartLegend.createHorizontalLabel index labelText "―" getLineColour


renderLegend :
    Int
    -> Int
    -> Maybe (List String)
    -> List (Svg msg)
renderLegend width height seriesLabels =
    let
        labels =
            ChartLegend.createLabels seriesLabels legendLabel
    in
        ChartLegend.renderBottomCenterAligned width height labels


generateSVGPathDesc :
    Int
    -> Int
    -> Float
    -> List ( Cell, Cell )
    -> List ( Cell, Cell )
    -> String
generateSVGPathDesc width height maxValue firstRow rowTuple =
    List.map (pointGenerator width height maxValue firstRow) rowTuple
        |> Shape.line Shape.linearCurve


pointGenerator :
    Int
    -> Int
    -> Float
    -> List ( Cell, Cell )
    -> ( String, String )
    -> Maybe ( Float, Float )
pointGenerator width height maxValue firstRow ( x, y ) =
    let
        ySantized =
            NumberParser.fromString y

        yScaleData =
            Scale.convert
                (yScale height maxValue)
                ySantized

        xScaleData =
            Scale.convert (xScale width firstRow) x
    in
        Just
            ( xScaleData
            , yScaleData
            )


xScale : Int -> List ( a1, a2 ) -> BandScale a1
xScale width firstRow =
    Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
        (List.map Tuple.first firstRow)
        ( 0, toFloat width - defaultChartPadding.totalHorizontal )


yScale : Int -> Float -> ContinuousScale
yScale height maxValue =
    Scale.linear ( 0, maxValue )
        ( (toFloat height) - defaultChartPadding.totalHorizontal, 0 )
