module Views.Widget.Renderers.BarChart exposing (render)

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


render : Widget Body -> Table.Data -> Html msg
render widget data =
    case widget.adapter of
        TABLE ->
            let
                ( headerRow, bodyRows, maxValue ) =
                    TableAdapter.adapt data

                dataAsHeaderValueTuples =
                    List.map (List.map2 (,) headerRow) bodyRows
            in
                div [ class "col-md-6 widget" ]
                    [ h3 [ Html.Attributes.title widget.description, Html.Attributes.class "heading" ] [ Html.text widget.name ]
                    , view dataAsHeaderValueTuples maxValue
                    , Utils.renderDataSourceInfoFrom widget
                    ]

        _ ->
            p [ class "data" ] [ Html.text "Sorry, I can only render bar charts from a TABLE adapter right now" ]


w : Float
w =
    Utils.mediumWidth


h : Float
h =
    Utils.mediumHeight


padding : Float
padding =
    Utils.mediumPadding


barColours : Array Color.Color
barColours =
    Array.fromList Scale.category10


getBarColour : Int -> String
getBarColour index =
    get index barColours
        |> Maybe.withDefault Color.black
        |> Color.Convert.colorToHex


xScale : List ( String, String ) -> BandScale String
xScale data =
    Scale.band
        { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
        (List.map Tuple.first data)
        ( 0, w - 2 * padding )


yScale : Float -> ContinuousScale
yScale maxValue =
    Scale.linear ( 0, maxValue ) ( h - 2 * padding, 0 )


xAxis : List ( String, String ) -> Svg msg
xAxis data =
    Axis.axis { defaultOptions | orientation = Axis.Bottom, tickFormat = Just Utils.formatStringTick } (Scale.toRenderable (xScale data))


yAxis : Float -> Svg msg
yAxis maxValue =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 5 } (yScale maxValue)


column : Int -> Int -> String -> BandScale String -> Float -> ( String, String ) -> Svg msg
column index totalRows colour xScaleBand maxValue ( header, value ) =
    let
        xpos =
            (Scale.convert xScaleBand header) + (colWidth * (toFloat index))

        xposText =
            (Scale.convert (Scale.toRenderable xScaleBand) header) + (colWidth * (toFloat index))

        ypos =
            Scale.convert (yScale maxValue) (String.toFloat value |> Result.withDefault 0)

        yposText =
            Scale.convert (yScale maxValue) (String.toFloat value |> Result.withDefault 0) - 5

        colWidth =
            Scale.bandwidth xScaleBand / toFloat (totalRows)

        colHeight =
            h - Scale.convert (yScale maxValue) (String.toFloat value |> Result.withDefault 0) - 2 * padding
    in
        g [ class "column" ]
            [ rect
                [ x <| toString xpos
                , y <| toString ypos
                , width <| toString colWidth
                , height <| toString colHeight
                , fill colour
                ]
                []
              -- TODO: Fix layering
              -- , text_
              --     [ x <| toString <| xposText
              --     , y <| toString <| yposText
              --     , textAnchor "middle"
              --     ]
              --     [ Svg.text <| toString value ]
            ]


view : List (List ( Cell, Cell )) -> Float -> Svg msg
view data maxValue =
    let
        firstDataTuple =
            List.head data |> Maybe.withDefault []

        indexedData =
            Array.toIndexedList (Array.fromList data)

        totalRows =
            List.length indexedData

        renderColumns num =
            List.map
                (\( index, row ) ->
                    renderColumn index totalRows row
                )
                indexedData

        renderColumn index totalRows row =
            g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series" ] <|
                List.map (column index totalRows (getBarColour index) (xScale row) maxValue) row
    in
        svg [ width (toString w ++ "px"), height (toString h ++ "px") ]
            (List.concat
                [ [ Svg.style []
                        [ Svg.text """
                            .column text { display: none; }
                            .column:hover rect { opacity: 0.7; }
                            .column:hover text { display: inline; z-index: 9999; }
                          """ ]
                  , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (h - padding) ++ ")") ]
                        [ xAxis firstDataTuple ]
                  , g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
                        [ (yAxis maxValue) ]
                  ]
                , renderColumns <| List.length data
                ]
            )
