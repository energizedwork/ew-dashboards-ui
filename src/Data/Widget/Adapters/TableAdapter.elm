module Data.Widget.Adapters.TableAdapter exposing (adapt, Orientation(..))

import Array
import Data.Widget.Adapters.CellRange as CellRange exposing (..)
import Data.Widget.Chart as Chart exposing (..)
import Data.Widget.Config as AdapterConfig
import Data.Widget.Table as Table exposing (Cell, Data, Row)
import Dict exposing (Dict)
import Json.Decode as Json exposing (Value)
import NumberParser


-- Possible values:
-- "bodyRows"
-- "xLabels"
-- "yLabels"


type Orientation
    = Vertical
    | Horizontal


adapt : AdapterConfig.Config -> Table.Data -> Orientation -> Chart.Data
adapt optionalConfig data orientation =
    let
        defaultXLabelsRange =
            case orientation of
                Vertical ->
                    Just <| CellRange.firstRowRange data

                Horizontal ->
                    Nothing

        defaultYLabelsRange =
            case orientation of
                Vertical ->
                    Nothing

                Horizontal ->
                    Just <| CellRange.firstColRange data

        defaultBodyRange =
            case orientation of
                Vertical ->
                    CellRange.remainingRowsRange data

                Horizontal ->
                    CellRange.remainingColsRange data

        xLabelsRange =
            let
                configXLabels =
                    Dict.get "xLabels" optionalConfig
            in
                case defaultXLabelsRange of
                    Just xRange ->
                        configXLabels
                            |> Maybe.withDefault (CellRange.encode xRange)
                            |> Json.decodeValue CellRange.decoder
                            |> Result.withDefault xRange
                            |> Just

                    Nothing ->
                        case configXLabels of
                            Just range ->
                                range
                                    |> Json.decodeValue CellRange.decoder
                                    |> Result.withDefault (CellRange.firstRowRange data)
                                    |> Just

                            Nothing ->
                                Nothing

        yLabelsRange =
            let
                configYLabels =
                    Dict.get "yLabels" optionalConfig
            in
                case defaultYLabelsRange of
                    Just yRange ->
                        configYLabels
                            |> Maybe.withDefault (CellRange.encode yRange)
                            |> Json.decodeValue CellRange.decoder
                            |> Result.withDefault yRange
                            |> Just

                    Nothing ->
                        case configYLabels of
                            Just range ->
                                range
                                    |> Json.decodeValue CellRange.decoder
                                    |> Result.withDefault (CellRange.firstColRange data)
                                    |> Just

                            Nothing ->
                                Nothing

        xLabels =
            case xLabelsRange of
                Just xRange ->
                    case orientation of
                        Vertical ->
                            Just <| CellRange.extractRow data xRange

                        Horizontal ->
                            Just <| CellRange.extractCol data xRange

                Nothing ->
                    Nothing

        yLabels =
            case yLabelsRange of
                Just yRange ->
                    case orientation of
                        Vertical ->
                            Just <| CellRange.extractRow data yRange

                        Horizontal ->
                            Just <| CellRange.extractCol data yRange

                Nothing ->
                    Nothing

        headerRow =
            Maybe.withDefault [] xLabels

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

        tableData =
            List.map (List.map2 (,) headerRow) bodyRows

        indexedTableData =
            Array.toIndexedList (Array.fromList tableData)

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

        noSeriesLabels =
            Nothing

        noXAxisLabel =
            Nothing

        noYAxisLabel =
            Nothing

        noForecastPosition =
            Nothing
    in
        Chart.Data
            bodyRows
            tableData
            indexedTableData
            minValue
            maxValue
            xLabels
            yLabels
            noSeriesLabels
            noXAxisLabel
            noYAxisLabel
            noForecastPosition
