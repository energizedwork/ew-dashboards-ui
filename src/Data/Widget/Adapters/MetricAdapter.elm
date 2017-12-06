module Data.Widget.Adapters.MetricAdapter exposing (defaultConfig, adapt)

import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition, asJsonValue, decoder)
import Data.Widget.Adapters.Config as AdapterConfig
import Data.Widget.Table as Table exposing (Data)
import Array
import Dict exposing (Dict)
import Json.Decode as Json exposing (Value)
import Json.Encode as Encode exposing (..)


sourceCellPosition : CellPosition
sourceCellPosition =
    ( 0, 0 )


targetCellPosition : CellPosition
targetCellPosition =
    ( 0, 1 )


defaultConfig : Dict String Json.Value
defaultConfig =
    Dict.fromList
        [ ( "sourceCell", CellPosition.asJsonValue sourceCellPosition )
        , ( "targetCell", CellPosition.asJsonValue targetCellPosition )
        ]


rowForCell : Array.Array (List String) -> CellPosition -> List String
rowForCell rows cellConfig =
    case Array.get (Tuple.first cellConfig) rows of
        Just row ->
            row

        Nothing ->
            []


valueForCell : Array.Array (List String) -> CellPosition -> String
valueForCell rows cell =
    let
        row =
            rowForCell rows cell
    in
        case Array.get (Tuple.second cell) (Array.fromList row) of
            Just value ->
                value

            Nothing ->
                ""


adapt : AdapterConfig.Config -> Data -> ( String, String )
adapt optionalConfig data =
    let
        rows =
            Array.fromList data.rows

        sourceCell =
            Dict.get "sourceCell" optionalConfig
                |> Maybe.withDefault (CellPosition.asJsonValue sourceCellPosition)
                |> Json.decodeValue CellPosition.decoder
                |> Result.withDefault sourceCellPosition

        targetCell =
            Dict.get "targetCell" optionalConfig
                |> Maybe.withDefault (CellPosition.asJsonValue targetCellPosition)
                |> Json.decodeValue CellPosition.decoder
                |> Result.withDefault targetCellPosition

        sourceValue =
            valueForCell rows sourceCell

        targetValue =
            valueForCell rows targetCell
    in
        ( sourceValue, targetValue )
