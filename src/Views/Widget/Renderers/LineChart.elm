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
import Views.Widget.Renderers.Config as ViewConfig exposing (defaultChartPadding, ChartPadding)
import Views.Widget.Renderers.Utils as Utils exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Visualization.Shape as Shape
import Views.Widget.Renderers.ChartLegend as ChartLegend
import Views.Widget.Renderers.ChartAxisLabels as ChartAxisLabels
import Views.Widget.Renderers.Chart as ChartRenderer
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

                namespace =
                    Utils.cssSafe widget.name
            in
                div
                    [ class <|
                        ViewConfig.colSpanClass optionalRendererConfig
                            ++ " widget"
                    ]
                    [ Utils.renderTitleFrom widget
                    , view namespace calculatedWidth calculatedHeight chartData
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


view : String -> Int -> Int -> Chart.Data -> Svg msg
view namespace w h chartData =
    let
        ( forecastPosition, requiresForecast ) =
            case chartData.forecastPosition of
                Just forecastPosition ->
                    ( forecastPosition - 1, True )

                Nothing ->
                    ( 0, False )

        actualsWidth =
            case requiresForecast of
                True ->
                    (chartDimensions.w // xTicksCount) * forecastPosition

                False ->
                    chartDimensions.w

        noPadding =
            Nothing

        forecastWidth =
            chartDimensions.w - (Debug.log "actualsWidth" actualsWidth)

        chartDimensions =
            ChartRenderer.calculateDimensions w h (Just padding)

        actualsDimensions =
            ChartRenderer.calculateDimensions actualsWidth chartDimensions.h noPadding

        forecastsDimensions =
            ChartRenderer.calculateDimensions forecastWidth chartDimensions.h noPadding

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

        padding =
            defaultChartPadding

        yGridTicks =
            Scale.ticks (yScale h chartData.maxValue padding) yTicksCount

        ( lineRenderer, forecastRenderer ) =
            let
                forecast =
                    True
            in
                case requiresForecast of
                    True ->
                        ( renderLines namespace w h chartData.maxValue firstRow indexedData defaultChartPadding (not forecast)
                        , renderLines namespace w h chartData.maxValue firstRow indexedData defaultChartPadding (forecast)
                        )

                    False ->
                        ( renderLines namespace w h chartData.maxValue firstRow indexedData defaultChartPadding (not forecast)
                        , []
                        )
    in
        svg [ Svg.Attributes.width (toString w ++ "px"), Svg.Attributes.height (toString h ++ "px") ]
            (List.concat
                [ [ ChartRenderer.renderClipPaths namespace actualsDimensions forecastsDimensions
                  , renderXAxis w h xTicksCount (xScale w firstRow padding) padding
                  , renderYAxis w h (yScale h chartData.maxValue padding) opts padding
                  , Utils.renderYGrid w h padding chartData.maxValue (yScale h chartData.maxValue padding) yGridTicks
                    -- , Utils.renderDebugGrid w h padding
                  ]
                , lineRenderer
                , forecastRenderer
                , renderLegend w h chartData.seriesLabels
                , [ ChartAxisLabels.renderXAxisLabel w h chartData.xAxisLabel padding ]
                , [ ChartAxisLabels.renderLeftYAxisLabel h chartData.yAxisLabel padding ]
                ]
            )


renderLines :
    String
    -> Int
    -> Int
    -> Float
    -> List ( Cell, Cell )
    -> List ( Int, List ( Cell, Cell ) )
    -> ChartPadding
    -> Bool
    -> List (Svg msg)
renderLines namespace width height maxValue firstRow indexedData chartPadding forecast =
    let
        scale =
            xScale width firstRow chartPadding
    in
        List.map
            (\( index, rowTuple ) ->
                generateSVGPathDesc width height maxValue firstRow rowTuple chartPadding
                    |> renderLine namespace (getLineColour (index)) scale chartPadding forecast
            )
            indexedData


renderLine : String -> String -> BandScale a1 -> ChartPadding -> Bool -> String -> Svg msg
renderLine namespace colour scale chartPadding forecast lineData =
    let
        clipPath =
            case forecast of
                True ->
                    "url(#" ++ namespace ++ "-right-region)"

                False ->
                    "url(#" ++ namespace ++ "-left-region)"

        firstTickPosition =
            Scale.bandwidth scale / 2

        gAtts =
            [ transform ("translate(" ++ (toString (chartPadding.left + firstTickPosition)) ++ ", " ++ toString chartPadding.top ++ ")")
            , class "series"
            , Svg.Attributes.clipPath clipPath
            ]

        pAtts =
            [ d lineData
            , stroke colour
            , strokeWidth "3px"
            , fill "none"
            ]

        ( groupAtts, pathAtts ) =
            case forecast of
                True ->
                    ( gAtts
                    , Svg.Attributes.strokeDasharray "5, 5" :: pAtts
                    )

                False ->
                    ( gAtts
                    , pAtts
                    )
    in
        g groupAtts
            [ Svg.path pathAtts []
            ]


renderXAxis : Int -> Int -> Int -> BandScale a1 -> ChartPadding -> Svg msg
renderXAxis width height numTicks bandScale chartPadding =
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

        translateAmount =
            ("translate("
                ++ toString (chartPadding.left - 1)
                ++ ", "
                ++ toString (toFloat height - chartPadding.bottom)
                ++ ")"
            )
    in
        g [ transform translateAmount ]
            [ xAxis ]


renderYAxis :
    Int
    -> b
    -> Axis.RenderableScale a domain range value
    -> Axis.Options value
    -> ChartPadding
    -> Svg msg
renderYAxis width height continuousScale opts chartPadding =
    let
        yAxis : Svg msg
        yAxis =
            Axis.axis { opts | tickFormat = Just Utils.formatNumberTick } continuousScale

        xTranslate =
            case opts.orientation of
                Axis.Left ->
                    floor <| chartPadding.left - 1

                Axis.Right ->
                    width - (floor (chartPadding.right) + 1)

                Axis.Top ->
                    floor <| chartPadding.top - 1

                Axis.Bottom ->
                    floor <| chartPadding.bottom - 1

        translateAmount =
            ("translate("
                ++ toString (xTranslate)
                ++ ", "
                ++ toString chartPadding.top
                ++ ")"
            )
    in
        g [ transform translateAmount ]
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
    -> ChartPadding
    -> String
generateSVGPathDesc width height maxValue firstRow rowTuple chartPadding =
    List.map (pointGenerator width height maxValue firstRow chartPadding) rowTuple
        |> Shape.line Shape.linearCurve


pointGenerator :
    Int
    -> Int
    -> Float
    -> List ( Cell, Cell )
    -> ChartPadding
    -> ( String, String )
    -> Maybe ( Float, Float )
pointGenerator width height maxValue firstRow chartPadding ( x, y ) =
    let
        ySantized =
            NumberParser.fromString y

        yScaleData =
            Scale.convert
                (yScale height maxValue chartPadding)
                ySantized

        xScaleData =
            Scale.convert (xScale width firstRow chartPadding) x
    in
        Just
            ( xScaleData
            , yScaleData
            )


xScale : Int -> List ( a1, a2 ) -> ChartPadding -> BandScale a1
xScale width firstRow chartPadding =
    Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2, align = 0.5 }
        (List.map Tuple.first firstRow)
        ( 0, toFloat width - chartPadding.totalHorizontal )


yScale : Int -> Float -> ChartPadding -> ContinuousScale
yScale height maxValue chartPadding =
    Scale.linear ( 0, maxValue )
        ( (toFloat height) - chartPadding.totalVertical, 0 )
