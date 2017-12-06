module Data.Widget.Adapters.LineAndBarAdapter exposing (defaultConfig, adapt)

import Array
import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition, asJsonValue, decoder)
import Data.Widget.Adapters.CellRange as CellRange exposing (..)
import Data.Widget.Adapters.Config as AdapterConfig
import Data.Widget.Table as Table exposing (Data, Row, Cell)
import Dict exposing (Dict)
import Json.Decode as Json exposing (Value)
import Json.Encode as Encode exposing (Value)
import List.Extra
import Views.Widget.Renderers.Utils as Utils exposing (..)


xLabelsIndex : Int
xLabelsIndex =
    0


yLabelsIndex : List Int
yLabelsIndex =
    [ 0 ]



-- Possible values:
-- "lineRows"
-- "barRows"
-- "xLabelsIndex"
-- "yLabelsIndicies"


defaultConfig : Dict String Json.Value
defaultConfig =
    Dict.fromList
        [ ( "xLabelsIndex", Encode.int xLabelsIndex )
        , ( "yLabelsIndicies", List.map (\y -> Encode.int y) yLabelsIndex |> Encode.list )
        ]


adapt : AdapterConfig.Config -> Data -> ( Row, List Row, List Row, Float, List String, List String )
adapt optionalConfig data =
    let
        actualXLabelsIndex =
            Dict.get "xLabelsIndex" optionalConfig
                |> Maybe.withDefault (Encode.int xLabelsIndex)
                |> Json.decodeValue Json.int
                |> Result.withDefault xLabelsIndex

        headerRow =
            case List.head data.rows of
                Just header ->
                    header

                Nothing ->
                    []

        xLabels =
            case List.Extra.getAt actualXLabelsIndex data.rows of
                Just xLabel ->
                    xLabel

                Nothing ->
                    []

        yLabels =
            case List.tail data.rows of
                Just body ->
                    body
                        |> List.map
                            (\row ->
                                List.head row |> Maybe.withDefault ""
                            )

                Nothing ->
                    []

        lineRows =
            case Dict.get "lineRows" optionalConfig of
                Just lineRows ->
                    let
                        range =
                            lineRows
                                |> Json.decodeValue CellRange.decoder
                                |> Result.withDefault [ ( 1, 1 ), ( 10, 2 ) ]
                    in
                        CellRange.extractRows data range

                Nothing ->
                    []

        barRows =
            case Dict.get "barRows" optionalConfig of
                Just barRows ->
                    let
                        range =
                            barRows
                                |> Json.decodeValue (Json.list CellPosition.decoder)
                                |> Result.withDefault [ ( 1, 3 ), ( 10, 4 ) ]
                    in
                        CellRange.extractRows data range

                Nothing ->
                    []

        bodyRows =
            case
                not (List.isEmpty lineRows)
                    && not (List.isEmpty barRows)
            of
                True ->
                    lineRows ++ barRows

                False ->
                    case List.tail data.rows of
                        Just body ->
                            body

                        Nothing ->
                            []

        ( lineChartRows, barChartRows ) =
            case
                not (List.isEmpty lineRows)
                    && not (List.isEmpty barRows)
            of
                True ->
                    ( lineRows, barRows )

                False ->
                    List.Extra.splitAt (totalDataRows // 2) bodyRows

        totalDataRows =
            List.length bodyRows

        rowMax row =
            List.maximum (Utils.rowToFloats row) |> Maybe.withDefault 0

        bodyRowMaxes =
            List.map rowMax bodyRows

        maxValue =
            (List.maximum bodyRowMaxes |> Maybe.withDefault 0)

        firstCellPerRow row =
            List.head row |> Maybe.withDefault ""
    in
        ( headerRow, lineChartRows, barChartRows, maxValue, xLabels, yLabels )
