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

        padding =
            defaultChartPadding

        yGridTicks =
            Scale.ticks (yScale h chartData.maxValue padding) yTicksCount

        -- TODO MSP read from config
        requiresForecast =
            True

        forecast =
            True

        clip =
            True

        ( lineRenderer, forecastRenderer ) =
            case requiresForecast of
                True ->
                    ( renderLines w h chartData.maxValue firstRow indexedData defaultChartPadding (not forecast) clip
                    , renderLines w h chartData.maxValue firstRow indexedData defaultChartPadding (forecast) clip
                    )

                False ->
                    ( renderLines w h chartData.maxValue firstRow indexedData defaultChartPadding (not forecast) (not clip)
                    , []
                    )
    in
        svg [ Svg.Attributes.width (toString w ++ "px"), Svg.Attributes.height (toString h ++ "px") ]
            (List.concat
                [ [ defs []
                        [ Svg.clipPath [ id "left-region" ]
                            [ Svg.rect
                                [ width (toString <| (w - floor padding.totalHorizontal) // 2)
                                , height (toString <| h - floor padding.top)
                                ]
                                []
                            ]
                        , Svg.clipPath [ id "right-region" ]
                            [ Svg.rect
                                [ width (toString <| w // 2)
                                , height (toString <| h - floor padding.top)
                                , x (toString <| (w - floor padding.totalHorizontal) // 2)
                                ]
                                []
                            ]
                        ]
                  ]
                , [ renderXAxis w h xTicksCount (xScale w firstRow defaultChartPadding) defaultChartPadding
                  , renderYAxis w h (yScale h chartData.maxValue defaultChartPadding) opts defaultChartPadding
                  , Utils.renderYGrid w h defaultChartPadding chartData.maxValue (yScale h chartData.maxValue defaultChartPadding) yGridTicks
                  ]
                , lineRenderer
                , forecastRenderer
                , renderLegend w h chartData.seriesLabels
                , [ ChartAxisLabels.renderXAxisLabel w h chartData.xAxisLabel defaultChartPadding ]
                , [ ChartAxisLabels.renderLeftYAxisLabel h chartData.yAxisLabel defaultChartPadding ]
                ]
            )


renderLines :
    Int
    -> Int
    -> Float
    -> List ( Cell, Cell )
    -> List ( Int, List ( Cell, Cell ) )
    -> ChartPadding
    -> Bool
    -> Bool
    -> List (Svg msg)
renderLines width height maxValue firstRow indexedData chartPadding forecast clip =
    List.map
        (\( index, rowTuple ) ->
            generateSVGPathDesc width height maxValue firstRow rowTuple chartPadding
                |> renderLine (getLineColour (index)) chartPadding forecast clip
        )
        indexedData


renderLine : String -> ChartPadding -> Bool -> Bool -> String -> Svg msg
renderLine colour chartPadding forecast clip lineData =
    let
        clipPath =
            case forecast of
                True ->
                    "url(#right-region)"

                False ->
                    "url(#left-region)"

        gAtts =
            [ transform ("translate(" ++ (toString (chartPadding.left + 25)) ++ ", " ++ toString chartPadding.top ++ ")")
            , class "series"
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
                    case clip of
                        True ->
                            ( Svg.Attributes.clipPath clipPath :: gAtts
                            , Svg.Attributes.strokeDasharray "5, 5" :: pAtts
                            )

                        False ->
                            ( gAtts
                            , Svg.Attributes.strokeDasharray "5, 5" :: pAtts
                            )

                False ->
                    case clip of
                        True ->
                            ( Svg.Attributes.clipPath clipPath :: gAtts
                            , pAtts
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
    in
        g [ transform ("translate(" ++ toString (chartPadding.left - 1) ++ ", " ++ toString (toFloat height - chartPadding.bottom) ++ ")") ]
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
    in
        g
            [ transform
                ("translate("
                    ++ toString (xTranslate)
                    ++ ", "
                    ++ toString chartPadding.top
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
    Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
        (List.map Tuple.first firstRow)
        ( 0, toFloat width - chartPadding.totalHorizontal )


yScale : Int -> Float -> ChartPadding -> ContinuousScale
yScale height maxValue chartPadding =
    Scale.linear ( 0, maxValue )
        ( (toFloat height) - chartPadding.totalVertical, 0 )
