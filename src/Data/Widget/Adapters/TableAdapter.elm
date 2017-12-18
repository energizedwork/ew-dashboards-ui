module Data.Widget.Adapters.TableAdapter exposing (defaultConfig, adapt)

import Data.Widget.Adapters.CellRange as CellRange exposing (..)
import Data.Widget.Adapters.Config as AdapterConfig
import Data.Widget.Table as Table exposing (Cell, Data, Row)
import Dict exposing (Dict)
import Json.Decode as Json exposing (Value)


-- Possible values:
-- "bodyRows"
-- "xLabels"


defaultConfig : Dict String Json.Value
defaultConfig =
    Dict.empty


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

        rowToInt row =
            List.map (\n -> String.toFloat n |> Result.withDefault 0) row

        rowMin row =
            List.minimum (rowToInt row) |> Maybe.withDefault 0

        rowMax row =
            List.maximum (rowToInt row) |> Maybe.withDefault 0

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
