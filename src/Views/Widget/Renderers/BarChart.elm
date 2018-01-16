module Views.Widget.Renderers.BarChart exposing (render, renderColumn, renderColumns, renderXAxis, renderYAxis)

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
                div [ class "col-md-6 widget" ]
                    [ h3 [ Html.Attributes.title widget.description, Html.Attributes.class "heading" ] [ Html.text widget.name ]
                    , view (width // 2) (height // 2) dataAsHeaderValueTuples maxValue
                    , Utils.renderDataSourceInfoFrom widget
                    ]

        _ ->
            p [ class "data" ] [ Html.text "Sorry, I can only render bar charts from a TABLE adapter right now" ]


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


xScale : Int -> List ( String, String ) -> BandScale String
xScale width data =
    Scale.band
        { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 }
        (List.map Tuple.first data)
        ( 0, toFloat width - 2 * padding )


yScale : Int -> Float -> ContinuousScale
yScale height maxValue =
    Scale.linear ( 0, maxValue ) ( toFloat height - 2 * padding, 0 )


xAxis : Int -> List ( String, String ) -> Svg msg
xAxis width data =
    Axis.axis { defaultOptions | orientation = Axis.Bottom, tickFormat = Just Utils.formatStringTick } (Scale.toRenderable (xScale width data))


yAxis : Int -> Float -> Svg msg
yAxis height maxValue =
    Axis.axis { defaultOptions | orientation = Axis.Left, tickCount = 5 } (yScale height maxValue)


column : Int -> Int -> Int -> String -> BandScale String -> Float -> ( String, String ) -> Svg msg
column height index totalRows colour xScaleBand maxValue ( header, value ) =
    let
        xpos =
            (Scale.convert xScaleBand header) + (colWidth * (toFloat index))

        xposText =
            (Scale.convert (Scale.toRenderable xScaleBand) header) + (colWidth * (toFloat index))

        ypos =
            Scale.convert (yScale height maxValue) (String.toFloat value |> Result.withDefault 0)

        yposText =
            Scale.convert (yScale height maxValue) (String.toFloat value |> Result.withDefault 0) - 5

        colWidth =
            Scale.bandwidth xScaleBand / toFloat (totalRows)

        colHeight =
            toFloat height - Scale.convert (yScale height maxValue) (String.toFloat value |> Result.withDefault 0) - 2 * padding
    in
        g [ class "column" ]
            [ rect
                [ x <| toString xpos
                , y <| toString ypos
                , Svg.Attributes.width <| toString colWidth
                , Svg.Attributes.height <| toString colHeight
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


view : Int -> Int -> List (List ( Cell, Cell )) -> Float -> Svg msg
view width height data maxValue =
    let
        firstDataTuple =
            List.head data |> Maybe.withDefault []

        indexedData =
            Array.toIndexedList (Array.fromList data)

        totalRows =
            List.length indexedData
    in
        svg [ Svg.Attributes.width (toString width ++ "px"), Svg.Attributes.height (toString height ++ "px") ]
            (List.concat
                [ [ Svg.style []
                        [ Svg.text """
                            .column text { display: none; }
                            .column:hover rect { opacity: 0.7; }
                            .column:hover text { display: inline; z-index: 9999; }
                          """ ]
                  , renderXAxis width height firstDataTuple
                  , renderYAxis width height maxValue
                  ]
                , renderColumns width height maxValue totalRows indexedData
                ]
            )


renderColumns :
    Int
    -> Int
    -> Float
    -> Int
    -> List ( Int, List ( String, String ) )
    -> List (Svg msg)
renderColumns width height maxValue totalRows indexedData =
    List.map
        (\( index, row ) ->
            renderColumn width height index totalRows row maxValue
        )
        indexedData


renderColumn :
    Int
    -> Int
    -> Int
    -> Int
    -> List ( String, String )
    -> Float
    -> Svg msg
renderColumn width height index totalRows row maxValue =
    g [ transform ("translate(" ++ toString padding ++ ", " ++ toString padding ++ ")"), class "series" ] <|
        List.map (column height index totalRows (getBarColour index) (xScale width row) maxValue) row


renderXAxis : Int -> Int -> List ( Cell, Cell ) -> Svg msg
renderXAxis width height firstDataTuple =
    g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString (toFloat height - padding) ++ ")") ]
        [ xAxis width firstDataTuple ]


renderYAxis : Int -> Int -> Float -> Svg msg
renderYAxis width height maxValue =
    g [ transform ("translate(" ++ toString (padding - 1) ++ ", " ++ toString padding ++ ")") ]
        [ (yAxis height maxValue) ]
