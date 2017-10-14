module Views.Widget.Renderers.HeatMap exposing (render, colourScaleFrom)

import Array exposing (..)
import Color.Convert
import Data.Widget as Widget exposing (Widget, Body)
import Data.Widget.Table as Table exposing (Data, Cell)
import Data.Widget.Adapters.Adapter exposing (Adapter(..))
import Data.Widget.Adapters.TableAdapter as TableAdapter
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Attributes exposing (attribute)
import Visualization.Axis as Axis exposing (defaultOptions)
import Visualization.Scale as Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)


render : Widget Body -> Table.Data -> Html msg
render widget data =
    case widget.adapter of
        TABLE ->
            let
                -- TODO: Own adapter? This is a bit leaky..
                ( headerRow, bodyRows, maxValue ) =
                    TableAdapter.adapt data

                dataAsHeaderValueTuples =
                    List.map (List.map2 (,) headerRow) bodyRows

                firstCellPerRow row =
                    List.head row |> Maybe.withDefault ""

                yLabels =
                    List.map firstCellPerRow bodyRows
            in
                view dataAsHeaderValueTuples yLabels maxValue

        _ ->
            p [ class "data" ] [ Html.text "Sorry, I can only render line charts from a TABLE adapter right now" ]


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    50


viewPortWidth : Float
viewPortWidth =
    w - padding


viewPortHeight : Float
viewPortHeight =
    h - padding


getCellColour : Float -> String
getCellColour index =
    Scale.infernoInterpolator index
        |> Color.Convert.colorToHex


colourScale : Float -> ContinuousScale
colourScale maxValue =
    Scale.linear ( 0, maxValue ) ( 0, 1 )


colourScaleFrom : String -> Float -> Float
colourScaleFrom amount maxValue =
    Scale.convert
        (colourScale maxValue)
        (String.toFloat amount |> Result.withDefault 0)


view : List (List ( Cell, Cell )) -> List String -> Float -> Svg msg
view data yLabels maxValue =
    let
        firstDataTuple =
            List.head data |> Maybe.withDefault []

        indexedData =
            Array.toIndexedList (Array.fromList data)

        totalRows =
            List.length indexedData

        totalCols =
            List.length firstDataTuple

        cellWidth =
            (viewPortWidth / toFloat totalCols)

        cellHeight =
            (viewPortHeight / toFloat totalRows)

        xScale : BandScale String
        xScale =
            Scale.band
                { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
                (List.map Tuple.first firstDataTuple)
                ( 0, viewPortWidth )

        yScale : BandScale String
        yScale =
            (Scale.band
                { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
                yLabels
                ( 0, viewPortHeight )
            )

        opts : Axis.Options a
        opts =
            Axis.defaultOptions

        xAxis : Svg msg
        xAxis =
            Axis.axis { opts | orientation = Axis.Bottom, tickCount = totalCols } (Scale.toRenderable xScale)

        yAxis : Svg msg
        yAxis =
            Axis.axis { opts | orientation = Axis.Left, tickCount = totalRows } (Scale.toRenderable yScale)

        squareClassAt rowIndex =
            case rowIndex of
                0 ->
                    "label"

                _ ->
                    "square"

        fillAt colIndex cell =
            case colIndex of
                -- 0 ->
                --     "#fff"
                _ ->
                    (getCellColour (colourScaleFrom cell maxValue))

        fontAt colIndex =
            case colIndex of
                0 ->
                    "#000"

                _ ->
                    "#fff"

        renderSquare rowIndex colIndex cell =
            g [ class <| squareClassAt colIndex ]
                [ rect
                    [ x <| toString (padding + toFloat colIndex * cellWidth)
                    , y <| toString (toFloat rowIndex * cellHeight)
                    , width <| toString cellWidth
                    , height <| toString cellHeight
                    , fill <| fillAt colIndex cell
                    , stroke "#fff"
                    , attribute "data-amount" cell
                    ]
                    []
                , text_
                    [ x <| toString <| (padding + toFloat colIndex * cellWidth) + (cellWidth / 2)
                    , y <| toString <| (toFloat rowIndex * cellHeight) + (cellHeight / 2)
                    , textAnchor "middle"
                    , alignmentBaseline "central"
                    , stroke "none"
                    , fill <| fontAt colIndex
                    ]
                    [ Svg.text <| cell ]
                ]

        renderSquares =
            List.map
                (\( rowIndex, row ) ->
                    g [ class "squares" ]
                        (List.map
                            (\( colIndex, ( header, cell ) ) ->
                                renderSquare rowIndex colIndex cell
                            )
                            (Array.toIndexedList (Array.fromList row))
                        )
                )
                indexedData
    in
        svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
            (List.concat
                [ [ Svg.style []
                        [ Svg.text """
                            .square text { display: none; }
                            .square:hover rect { opacity: 0.7; }
                            .square:hover text { display: inline; }
                          """ ]
                  , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
                        [ xAxis ]
                  , g [ transform ("translate(" ++ toString (padding - 1) ++ ",0)") ]
                        [ yAxis ]
                  ]
                , renderSquares
                ]
            )