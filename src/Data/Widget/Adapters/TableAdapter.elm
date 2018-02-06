module Data.Widget.Adapters.TableAdapter exposing (adapt)

import Data.Widget.Adapters.CellRange as CellRange exposing (..)
import Data.Widget.Config as AdapterConfig
import Data.Widget.Table as Table exposing (Cell, Data, Row)
import Dict exposing (Dict)
import Json.Decode as Json exposing (Value)
import NumberParser


-- Possible values:
-- "bodyRows"
-- "xLabels"
-- TODO refactor to use Chart.Data


adapt : AdapterConfig.Config -> Data -> ( Row, List Row, Float, Float, List String )
adapt optionalConfig data =
    let
        defaultHeaderRange =
            CellRange.firstRowRange data

        defaultBodyRange =
            CellRange.remainingRowsRange data

        headerRange =
            Dict.get "xLabels" optionalConfig
                |> Maybe.withDefault (CellRange.encode defaultHeaderRange)
                |> Json.decodeValue CellRange.decoder
                |> Result.withDefault defaultHeaderRange

        xLabels =
            CellRange.extractRow data headerRange

        headerRow =
            xLabels

        bodyRows =
            case Dict.get "bodyRows" optionalConfig of
                Just bodyRows ->
                    let
                        range =
                            bodyRows
                                |> Json.decodeValue CellRange.decoder
                                |> Result.withDefault defaultBodyRange
                    in
                        CellRange.extractRows data range

                Nothing ->
                    CellRange.extractRows data defaultBodyRange

        rowToNumber row =
            List.map NumberParser.fromString row

        rowMin row =
            List.minimum (rowToNumber row) |> Maybe.withDefault 0

        rowMax row =
            List.maximum (rowToNumber row) |> Maybe.withDefault 0

        bodyRowMins =
            List.map rowMin bodyRows

        bodyRowMaxes =
            List.map rowMax bodyRows

        minValue =
            (List.minimum bodyRowMins |> Maybe.withDefault 0)

        maxValue =
            (List.maximum bodyRowMaxes |> Maybe.withDefault 0)
    in
        ( headerRow, bodyRows, minValue, maxValue, xLabels )
