module Data.Widget.Adapters.MetricAdapter exposing (defaultConfig, adapt)

import Data.Widget.Adapters.CellPosition as CellPosition exposing (CellPosition(..), encode, decoder)
import Data.Widget.Config as AdapterConfig
import Data.Widget.Table as Table exposing (Data)
import Array
import Dict exposing (Dict)
import Json.Decode as Json exposing (Value)


-- TODO These are flipped (y, x) and zero based which is unique amongst the adapters


sourceCellPosition : CellPosition
sourceCellPosition =
    CellPosition ( 0, 0 )


targetCellPosition : CellPosition
targetCellPosition =
    CellPosition ( 0, 1 )


defaultConfig : Dict String Json.Value
defaultConfig =
    Dict.fromList
        [ ( "sourceCell", CellPosition.encode sourceCellPosition )
        , ( "targetCell", CellPosition.encode targetCellPosition )
        ]


rowForCell : Array.Array (List String) -> CellPosition -> List String
rowForCell rows cellPosition =
    case Array.get (CellPosition.x cellPosition) rows of
        Just row ->
            row

        Nothing ->
            []


valueForCell : Array.Array (List String) -> CellPosition -> String
valueForCell rows cellPosition =
    let
        row =
            rowForCell rows cellPosition
    in
        case Array.get (CellPosition.y cellPosition) (Array.fromList row) of
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
                |> Maybe.withDefault (CellPosition.encode sourceCellPosition)
                |> Json.decodeValue CellPosition.decoder
                |> Result.withDefault sourceCellPosition

        targetCell =
            Dict.get "targetCell" optionalConfig
                |> Maybe.withDefault (CellPosition.encode targetCellPosition)
                |> Json.decodeValue CellPosition.decoder
                |> Result.withDefault targetCellPosition

        sourceValue =
            valueForCell rows sourceCell

        targetValue =
            valueForCell rows targetCell
    in
        ( sourceValue, targetValue )
