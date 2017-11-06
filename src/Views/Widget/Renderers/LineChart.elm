module Views.Widget.Renderers.LineChart exposing (render)

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
        TABLE ->
            let
                ( headerRow, bodyRows, maxValue ) =
                    TableAdapter.adapt data

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

        xScale : BandScale String
        xScale =
            Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } (List.map Tuple.first firstDataTuple) ( 0, toFloat width - 2 * padding )

        yScale : ContinuousScale
        yScale =
            Scale.linear ( 0, maxValue ) ( (toFloat height) - 2 * padding, 0 )

        opts : Axis.Options a
        opts =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Axis.axis { opts | orientation = Axis.Bottom, tickFormat = Just Utils.formatStringTick, tickCount = List.length firstDataTuple } (Scale.toRenderable xScale)

        yAxis : Svg msg
        yAxis =
            Axis.axis { opts | orientation = Axis.Left, tickCount = 5 } yScale

        areaGenerator : ( String, String ) -> Maybe ( ( Float, Float ), ( Float, Float ) )
        areaGenerator ( x, y ) =
            Just ( ( Scale.convert xScale x, Tuple.first (Scale.rangeExtent yScale) ), ( Scale.convert xScale x, Scale.convert yScale (String.toFloat y |> Result.withDefault 0) ) )

        lineGenerator : ( String, String ) -> Maybe ( Float, Float )
        lineGenerator ( x, y ) =
            Just ( Scale.convert xScale x, Scale.convert yScale (String.toFloat y |> Result.withDefault 0) )

        generateAreaData : List ( String, String ) -> String
        generateAreaData dataTuple =
            List.map areaGenerator dataTuple
                |> Shape.area Shape.linearCurve

        generateLineData : List ( String, String ) -> String
        generateLineData dataTuple =
            List.map lineGenerator dataTuple
                |> Shape.line Shape.linearCurve

        renderLine colour lineData =
            g [ transform ("translate(78" ++ ", " ++ toString padding ++ ")"), class "series" ]
                [ Svg.path [ d lineData, stroke colour, strokeWidth "3px", fill "none" ] []
                  -- , Svg.path [ d area, stroke "none", strokeWidth "3px", fill "rgba(255, 0, 0, 0.54)" ] []
                ]

        renderLines =
            List.map
                (\( index, dataTuple ) ->
                    generateLineData dataTuple
                        |> renderLine (getLineColour (index))
                )
                indexedData
    in
        svg [ Svg.Attributes.width (toString width ++ "px"), Svg.Attributes.height (toString height ++ "px") ]
            (List.concat
                [ [ g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (toFloat height - padding) ++ ")") ]
                        [ xAxis ]
                  , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
                        [ yAxis ]
                  ]
                , renderLines
                ]
            )
