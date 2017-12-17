module Data.Widget.Adapters.TableAdapter exposing (defaultConfig, adapt)

import Data.Widget.Adapters.CellPosition exposing (CellPosition(..))
import Data.Widget.Adapters.CellRange as CellRange exposing (..)
import Data.Widget.Adapters.Config as AdapterConfig
import Data.Widget.Table as Table exposing (Cell, Data, Row)
import Dict exposing (Dict)
import Json.Decode as Json exposing (Value)
import Json.Encode as Encode exposing (Value)
import List.Extra


xLabelsIndex : Int
xLabelsIndex =
    1



-- Possible values:
-- "bodyRows"
-- "xLabelsIndex"
-- TODO Change to use CellRange, not an index


defaultConfig : Dict String Json.Value
defaultConfig =
    Dict.fromList
        [ ( "xLabelsIndex", Encode.int xLabelsIndex ) ]



-- Dict.fromList
--     [ ( "xLabels"
--       , CellRange.encode <|
--             CellRange
--                 (CellPosition ( 1, 1 ))
--                 (CellPosition ( 10, 1 ))
--       )
--     ]


adapt : AdapterConfig.Config -> Data -> ( Row, List Row, Float, Float, List String )
adapt optionalConfig data =
    let
        actualXLabelsIndex =
            (Dict.get "xLabelsIndex" optionalConfig
                |> Maybe.withDefault (Encode.int xLabelsIndex)
                |> Json.decodeValue Json.int
                |> Result.withDefault xLabelsIndex
            )
                - 1

        -- TODO should be CellRange.extractRows data headerRange
        xLabels =
            case List.Extra.getAt actualXLabelsIndex data.rows of
                Just xLabel ->
                    xLabel

                Nothing ->
                    []

        headerRow =
            xLabels

        bodyRows =
            case Dict.get "bodyRows" optionalConfig of
                Just bodyRows ->
                    let
                        range =
                            bodyRows
                                |> Json.decodeValue CellRange.decoder
                                |> Result.withDefault
                                    (CellRange
                                        (CellPosition ( 1, 1 ))
                                        (CellPosition ( 10, 2 ))
                                    )
                    in
                        CellRange.extractRows data range

                Nothing ->
                    case List.tail data.rows of
                        Just body ->
                            body

                        Nothing ->
                            []

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
