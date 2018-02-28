module Data.Widget.Adapters.TableAdapter exposing (adapt, Orientation(..))

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


type Orientation
    = Vertical
    | Horizontal


adapt : AdapterConfig.Config -> Data -> Orientation -> ( Row, List Row, Float, Float, Row )
adapt optionalConfig data orientation =
    let
        defaultHeaderRange =
            case orientation of
                Vertical ->
            CellRange.firstRowRange data

                Horizontal ->
                    CellRange.firstColRange data

        defaultBodyRange =
            case orientation of
                Vertical ->
            CellRange.remainingRowsRange data

                Horizontal ->
                    CellRange.remainingColsRange data

        headerRange =
            Dict.get "xLabels" optionalConfig
                |> Maybe.withDefault (CellRange.encode defaultHeaderRange)
                |> Json.decodeValue CellRange.decoder
                |> Result.withDefault defaultHeaderRange

        xLabels =
            case orientation of
                Vertical ->
            CellRange.extractRow data headerRange

                Horizontal ->
                    CellRange.extractCol data headerRange

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
                        case orientation of
                            Vertical ->
                        CellRange.extractRows data range

                            Horizontal ->
                                CellRange.extractCols data range

                Nothing ->
                    case orientation of
                        Vertical ->
                    CellRange.extractRows data defaultBodyRange

                        Horizontal ->
                            CellRange.extractCols data defaultBodyRange

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
