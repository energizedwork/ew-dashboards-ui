module Views.Widget.Renderers.LineChart
    exposing
        ( render
        , renderLines
        , renderLine
        , renderXAxis
        , renderYAxis
        , renderYGrid
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
import Html.Attributes exposing (title)
import NumberParser
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Views.Widget.Renderers.Config as ViewConfig
import Views.Widget.Renderers.Utils as Utils exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Visualization.Shape as Shape
import Views.Widget.Renderers.ChartLegend as ChartLegend


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
                    [ h3
                        [ Html.Attributes.title widget.description
                        , Html.Attributes.class "heading"
                        ]
                        [ Html.text widget.name ]
                    , view calculatedWidth calculatedHeight chartData.data chartData.maxValue chartData.seriesLabels
                    , Utils.renderDataSourceInfoFrom widget
                    ]

        _ ->
            p [ class "data" ] [ Html.text "Sorry, I can only render line charts from a CHART adapter right now" ]


padding : Float
padding =
    Utils.mediumPadding


lineColours : Array Color.Color
lineColours =
    Array.fromList (List.reverse Scale.category10)


getLineColour : Int -> String
getLineColour index =
    get index lineColours
        |> Maybe.withDefault Color.black
        |> Color.Convert.colorToHex


view : Int -> Int -> List (List ( Cell, Cell )) -> Float -> Maybe (List String) -> Svg msg
view w h data maxValue seriesLabels =
    let
        firstRow =
            List.head data
                |> Maybe.withDefault []

        indexedData =
            Array.toIndexedList (Array.fromList data)

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
    in
        svg [ Svg.Attributes.width (toString w ++ "px"), Svg.Attributes.height (toString h ++ "px") ]
            (List.concat
                [ [ renderXAxis w h xTicksCount (xScale w firstRow)
                  , renderYAxis w h (yScale h maxValue) opts
                  , renderYGrid w h maxValue <| Scale.ticks (yScale h maxValue) yTicksCount
                  ]
                , renderLines w h maxValue firstRow indexedData
                , renderLegend h seriesLabels
                ]
            )


renderYGrid : Int -> Int -> Float -> List Float -> Svg msg
renderYGrid width height maxValue ticks =
    g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (padding) ++ ")") ] <|
        List.indexedMap (yGridLine width height maxValue) ticks


yGridLine : Int -> Int -> Float -> Int -> Float -> Svg msg
yGridLine width height maxValue index tick =
    let
        yPos =
            (toString (Scale.convert (yScale height maxValue) tick))
    in
        line
            [ x1 "0"
            , y1 yPos
            , x2 <| toString (toFloat width - 2 * padding)
            , y2 yPos
            , stroke "#ccc"
            , strokeWidth "1"

            -- , strokeWidth (toString (Basics.max (toFloat (index % 2)) 0.5))
            ]
            []


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
    g [ transform ("translate(78" ++ ", " ++ toString padding ++ ")"), class "series" ]
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
        g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (toFloat height - padding) ++ ")") ]
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
                    floor <| padding - 1

                Axis.Right ->
                    width - (floor (padding) + 1)

                Axis.Top ->
                    floor <| padding - 1

                Axis.Bottom ->
                    floor <| padding - 1
    in
        g
            [ transform
                ("translate("
                    ++ toString (xTranslate)
                    ++ ", "
                    ++ toString padding
                    ++ ")"
                )
            ]
            [ yAxis ]


legendLabel : Int -> String -> Svg msg
legendLabel index labelText =
    ChartLegend.createHorizontalLabel index labelText "―" getLineColour


renderLegend :
    Int
    -> Maybe (List String)
    -> List (Svg msg)
renderLegend top seriesLabels =
    let
        labels =
            ChartLegend.createLabels seriesLabels legendLabel
    in
        ChartLegend.render top labels padding


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
        ( 0, toFloat width - 2 * padding )


yScale : Int -> Float -> ContinuousScale
yScale height maxValue =
    Scale.linear ( 0, maxValue )
        ( (toFloat height) - 2 * padding, 0 )
