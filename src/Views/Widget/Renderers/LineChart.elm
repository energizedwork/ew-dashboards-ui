module Views.Widget.Renderers.LineChart exposing (render, renderLines, renderLine, renderXAxis, renderYAxis)

import Array exposing (..)
import Color
import Color.Convert
import Data.Widget as Widget exposing (Widget, Body)
import Data.Widget.Table as Table exposing (Data, Cell)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.TableAdapter as TableAdapter
import Html exposing (..)
import Html.Attributes exposing (title)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Views.Widget.Renderers.Utils as Utils exposing (..)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Visualization.Shape as Shape


render : Int -> Int -> Widget Body -> Table.Data -> Html msg
render width height widget data =
    case widget.adapter of
        TABLE optionalConfig ->
            let
                ( headerRow, bodyRows, minValue, maxValue, xLabels ) =
                    TableAdapter.adapt optionalConfig data

                dataAsHeaderValueTuples =
                    List.map (List.map2 (,) headerRow) bodyRows
            in
                div [ class "col-md-12 widget" ]
                    [ h3 [ Html.Attributes.title widget.description, Html.Attributes.class "heading" ] [ Html.text widget.name ]
                    , view width (height // 2) dataAsHeaderValueTuples maxValue
                    , Utils.renderDataSourceInfoFrom widget
                    ]

        _ ->
            p [ class "data" ] [ Html.text "Sorry, I can only render line charts from a TABLE adapter right now" ]


padding : Float
padding =
    Utils.mediumPadding


lineColours : Array Color.Color
lineColours =
    Array.fromList Scale.category10


getLineColour : Int -> String
getLineColour index =
    get index lineColours
        |> Maybe.withDefault Color.black
        |> Color.Convert.colorToHex


view : Int -> Int -> List (List ( Cell, Cell )) -> Float -> Svg msg
view width height data maxValue =
    let
        firstDataTuple =
            List.head data |> Maybe.withDefault []

        indexedData =
            Array.toIndexedList (Array.fromList data)

        opts : Axis.Options a
        opts =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Axis.axis
                { opts
                    | orientation = Axis.Bottom
                    , tickFormat = Just Utils.formatStringTick
                    , tickCount = List.length firstDataTuple
                }
                (Scale.toRenderable <| xScale width firstDataTuple)

        yAxis : Svg msg
        yAxis =
            Axis.axis { opts | orientation = Axis.Left, tickCount = 5 }
                (yScale height maxValue)
    in
        svg [ Svg.Attributes.width (toString width ++ "px"), Svg.Attributes.height (toString height ++ "px") ]
            (List.concat
                [ [ renderXAxis width height xAxis
                  , renderYAxis width height yAxis
                  ]
                , renderLines width height maxValue firstDataTuple indexedData
                ]
            )


renderLines :
    Int
    -> Int
    -> Float
    -> List ( Cell, Cell )
    -> List ( Int, List ( Cell, Cell ) )
    -> List (Svg msg)
renderLines width height maxValue firstDataTuple indexedData =
    List.map
        (\( index, dataTuple ) ->
            generateLineData width height maxValue firstDataTuple dataTuple
                |> renderLine (getLineColour (index))
        )
        indexedData


renderLine : String -> String -> Svg msg
renderLine colour lineData =
    g [ transform ("translate(78" ++ ", " ++ toString padding ++ ")"), class "series" ]
        [ Svg.path [ d lineData, stroke colour, strokeWidth "3px", fill "none" ] []
        ]


renderXAxis : a -> Int -> Svg msg -> Svg msg
renderXAxis width height xAxis =
    g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (toFloat height - padding) ++ ")") ]
        [ xAxis ]


renderYAxis : a -> b -> Svg msg -> Svg msg
renderYAxis width height yAxis =
    g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
        [ yAxis ]


generateLineData :
    Int
    -> Int
    -> Float
    -> List ( Cell, Cell )
    -> List ( Cell, Cell )
    -> String
generateLineData width height maxValue firstDataTuple dataTuple =
    List.map (lineGenerator width height maxValue firstDataTuple) dataTuple
        |> Shape.line Shape.linearCurve


lineGenerator :
    Int
    -> Int
    -> Float
    -> List ( Cell, Cell )
    -> ( String, String )
    -> Maybe ( Float, Float )
lineGenerator width height maxValue firstDataTuple ( x, y ) =
    Just
        ( Scale.convert (xScale width firstDataTuple) x
        , Scale.convert (yScale height maxValue) (String.toFloat y |> Result.withDefault 0)
        )


xScale : Int -> List ( a1, a2 ) -> BandScale a1
xScale width firstDataTuple =
    Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
        (List.map Tuple.first firstDataTuple)
        ( 0, toFloat width - 2 * padding )


yScale : Int -> Float -> ContinuousScale
yScale height maxValue =
    Scale.linear ( 0, maxValue ) ( (toFloat height) - 2 * padding, 0 )
